namespace SilenceDetect

open System

type Interval =
    { Start: TimeSpan
      Stop: TimeSpan
      Length: TimeSpan }


type VolumeInterval =
    | Silence of Interval
    | Sound of Interval


[<RequireQualifiedAccess>]
module VolumeInterval =    
    open NAudio.Wave
    open Common

    let create (start: TimeSpan) (stop: TimeSpan) =
        { Start = start
          Stop = stop
          Length = stop - start }

    let empty =
        { Start = TimeSpan.Zero
          Stop = TimeSpan.Zero
          Length = TimeSpan.Zero }

    let cropSilences
        (maxSilenceDuration: float)
        (intervals: VolumeInterval list)
        (markSilences: bool)
        (reader: AudioFileReader)        
        =

        let samplesPerSecond = sps reader

        let span2samples (span: TimeSpan) =
            span.TotalSeconds * float samplesPerSecond |> int

        let silenceReplacement =
            let s1 = Array.create 10 0.35f
            let s2 = Array.create 10 -0.35f

            let emptyness =
                Array.create (int (maxSilenceDuration * float samplesPerSecond)) (0.0f)

            if markSilences then
                [| s1; emptyness; s2 |] |> Array.concat
            else
                emptyness

        reader.Position <- 0
        
        seq {
            for sample in intervals do
                match sample with
                | Silence range ->
                    reader.CurrentTime <- reader.CurrentTime.Add(range.Length)

                    if (range.Length.TotalSeconds > maxSilenceDuration) then
                        yield! silenceReplacement
                        
                        // writer.WriteSamples(silenceReplacement, 0, silenceReplacement.Length)

                | Sound range ->
                    let count = range.Length |> span2samples

                    yield! reader |> AFR.mapSamples count 
                    
        }

    let writeFileWithSilenceCropped
        (outputFile: string)
        (maxSilenceDuration: float)
        (markSilences: bool)
        (intervals: VolumeInterval list)
        (reader: AudioFileReader)
        =

        let samplesPerSecond = sps reader

        let span2samples (span: TimeSpan) =
            span.TotalSeconds * float samplesPerSecond |> int

        let silenceReplacement =
            let s1 = Array.create 10 0.35f
            let s2 = Array.create 10 -0.35f

            let emptyness =
                Array.create (int (maxSilenceDuration * float samplesPerSecond)) (0.0f)

            if markSilences then
                [| s1; emptyness; s2 |] |> Array.concat
            else
                emptyness

        reader.Position <- 0
        use writer = new WaveFileWriter(outputFile, reader.WaveFormat)

        List.fold
            (fun state current ->
                match current with
                | Silence range ->
                    reader.CurrentTime <- reader.CurrentTime.Add(range.Length)

                    if (range.Length.TotalSeconds > maxSilenceDuration) then
                        writer.WriteSamples(silenceReplacement, 0, silenceReplacement.Length)

                    reader
                | Sound range ->
                    let count = range.Length |> span2samples

                    reader
                    |> AFR.TakeSamples
                        (fun buff ->
                            let samples = buff |> Array.ofSeq
                            writer.WriteSamples(samples, 0, samples.Length))
                        count)
            reader
            intervals
        |> ignore

        writer.Flush()

        writer.TotalTime

    let padSound (padding: TimeSpan) (intervals: VolumeInterval list) =

        let doublePadding = TimeSpan.FromMilliseconds(padding.TotalMilliseconds * 2.0)
        let first = intervals |> List.head
        let last = intervals |> List.last

        intervals
        |> List.map (function
            | range when range = first ->
                match range with
                | Sound snd ->
                    Sound
                        { snd with
                            Stop = snd.Stop + padding
                            Length = snd.Length + padding }
                | Silence slc ->
                    Silence
                        { slc with
                            Stop = slc.Stop - padding
                            Length = slc.Length - padding }
            | range when range = last ->
                match range with
                | Sound snd ->
                    Sound
                        { snd with
                            Start = snd.Start + padding
                            Length = snd.Length + padding }
                | Silence slc ->
                    Silence
                        { slc with
                            Start = slc.Start - padding
                            Length = slc.Length - padding }
            | Sound sound ->
                Sound
                    { Start = sound.Start - padding
                      Stop = sound.Stop + padding
                      Length = sound.Length + doublePadding }
            | Silence silence ->
                Silence
                    { Start = silence.Start + padding
                      Stop = silence.Stop - padding
                      Length = silence.Length - doublePadding })

    let createFileWithSilenceMarked (outputPath: string) (reader: AudioFileReader) (intervals: VolumeInterval list) =

        let s1 = Array.create 10 0.35f |> Array.collect (BitConverter.GetBytes)
        let s2 = Array.create 10 -0.35f |> Array.collect (BitConverter.GetBytes)

        reader.Position <- 0
        use writer = new WaveFileWriter(outputPath, reader.WaveFormat)

        intervals
        |> List.iter (fun range ->
            match range with
            | Silence silence ->
                let count = silence.Length |> span2bytes reader

                reader
                |> AFR.TakeBytes
                    (fun buff ->
                        let samples = buff |> Array.ofSeq
                        let toWrite = [| s1; samples; s2 |] |> Array.concat
                        writer.Write(toWrite, 0, toWrite.Length))
                    count
                |> ignore
            | Sound sound ->
                let count = sound.Length |> span2bytes reader

                reader
                |> AFR.TakeBytes
                    (fun buff ->
                        let samples = buff |> Array.ofSeq
                        writer.Write(samples, 0, samples.Length))
                    count
                |> ignore)

        writer.Flush()

        writer.TotalTime
