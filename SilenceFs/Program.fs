namespace Silence


open System
open System.IO
open NAudio.Wave
open CommandLine
open Model
open Newtonsoft.Json
open Common


module Program =



    let overwriteSilences
        (reader: AudioFileReader)
        (outputFile: string)
        (maxSilenceDuration: float)
        (ranges: Range list)
        (markSilences: bool)
        =

        let samplesPerSecond = sps reader

        let span2samples (span: TimeSpan) =
            span.TotalSeconds * float samplesPerSecond |> int

        let s1 = Array.create 10 0.35f
        let s2 = Array.create 10 -0.35f

        let shortSilence =
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
                    //let count = range.Length |> span2samples
                    // reader |> AFR.SkipSamples count

                    reader.CurrentTime <- reader.CurrentTime.Add(range.Length)

                    if (range.Length.TotalSeconds > maxSilenceDuration) then
                        writer.WriteSamples(shortSilence, 0, shortSilence.Length)

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
            ranges
        |> ignore

        writer.Flush()

        writer.TotalTime

    let markSilences (markedPath: string) (maxSilenceDuration: float) (reader: AudioFileReader) (ranges: Range list) =
        let samplesPerSecond = sps reader

        let span2samples (span: TimeSpan) =
            (span.TotalSeconds * float samplesPerSecond) |> int

        let span2bytes (span: TimeSpan) =
            (span.TotalSeconds * float reader.WaveFormat.AverageBytesPerSecond) |> int

        let s1 = Array.create 10 0.35f |> Array.collect (BitConverter.GetBytes)
        let s2 = Array.create 10 -0.35f |> Array.collect (BitConverter.GetBytes)

        reader.Position <- 0
        use writer = new WaveFileWriter(markedPath, reader.WaveFormat)

        ranges
        |> List.iter (fun range ->
            match range with
            | Silence silence ->
                let count = silence.Length |> span2bytes

                reader
                |> AFR.TakeBytes
                    (fun buff ->
                        let samples = buff |> Array.ofSeq
                        let toWrite = [| s1; samples; s2 |] |> Array.concat
                        writer.Write(toWrite, 0, toWrite.Length))
                    count
                |> ignore
            | Sound sound ->
                let count = sound.Length |> span2bytes

                reader
                |> AFR.TakeBytes
                    (fun buff ->
                        let samples = buff |> Array.ofSeq
                        writer.Write(samples, 0, samples.Length))
                    count
                |> ignore)

        writer.Flush()

        writer.TotalTime


    let report (ranges: Range list) (duration: TimeSpan) (originalDuration: TimeSpan) (opts: DetectOptions) =

        let maxSilenceSpan = TimeSpan.FromSeconds(opts.MaxSilenceDuration)

        let silenceCount =
            ranges
            |> List.filter (function
                | Silence x when x.Length >= maxSilenceSpan -> true
                | _ -> false)
            |> List.length

        let (silenceDuration, soundDuration) =
            ranges
            |> List.fold
                (fun (silence, sound) current ->
                    match current with
                    | Silence x -> (silence + x.Length, sound)
                    | Sound x -> (silence, sound + x.Length))
                (TimeSpan.Zero, TimeSpan.Zero)

        {| SilenceDuration = silenceDuration
           SoundDuration = soundDuration
           ExpectedInputDuration = silenceDuration + soundDuration
           ActualInputDuration = originalDuration
           ExpectedOutputDuration =
            soundDuration
            + float silenceCount * TimeSpan.FromSeconds(opts.MaxSilenceDuration)
           ActualOutputDuration = duration
           OutputFile =
            if isNotNullorEmpty opts.OutputFile then
                IO.Path.GetFullPath(opts.OutputFile)
            else
                ""

           Ranges =
            ranges
            |> List.map (function
                | Silence x -> {| x with Case = "Silence" |}
                | Sound x -> {| x with Case = "Sound" |}) |}
        |> fun x -> JsonConvert.SerializeObject(x, Formatting.Indented)
        |> printfn "%s"

    let DetectSilence (opts: DetectOptions) =
        //printfn "%A" opts
        let offset = TimeSpan.FromSeconds(opts.Offset)
        let duration = opts.Duration |> Option.map (fun dur -> TimeSpan.FromSeconds(dur))

        use reader = new AudioFileReader(opts.InputFile)

        let samplesPerSecond = reader.WaveFormat.SampleRate * reader.WaveFormat.Channels
        let bin = TimeSpan.FromMilliseconds(opts.Sampling)
        let samplesPerBin = float samplesPerSecond * bin.TotalSeconds

        let indexedBinnedSamples =
            reader
            |> AFR.Drain offset duration
            |> Seq.chunkBySize (int samplesPerBin)
            |> Seq.map (Array.map (Math.Abs) >> Array.average >> float)
            |> Seq.mapi (fun i avg ->
                {| TimeStamp =
                    TimeSpan.FromMilliseconds(
                        float i * bin.TotalMilliseconds + offset.TotalMilliseconds + opts.Sampling
                    )
                   Sample = avg                
                |})
            |> List.ofSeq

        let ranges =
            let mutable silence = false
            let volumeTreshold = 0.0075
            let maxSilenceDuration = TimeSpan.FromSeconds(opts.MaxSilenceDuration)

            let mutable currentSilence = { RangeData.Empty with Start = offset }
            let mutable currentSound = { RangeData.Empty with Start = offset }

            let finalSample = indexedBinnedSamples |> List.last

            seq {
                for sample in indexedBinnedSamples do
                    match sample.Sample <= volumeTreshold with
                    // End sound, start silence
                    | true when silence = false ->
                        silence <- true

                        currentSilence <-
                            { currentSilence with
                                Start = sample.TimeStamp
                                Stop = reader.TotalTime }

                    // End silence, start sound
                    | false when silence = true ->
                        silence <- false

                        currentSilence <-
                            { currentSilence with
                                Stop = sample.TimeStamp
                                Length = sample.TimeStamp - currentSilence.Start }

                        if currentSilence.Length >= maxSilenceDuration then
                            yield
                                Sound
                                    { currentSound with
                                        Stop = currentSilence.Start
                                        Length = currentSilence.Start - currentSound.Start }

                            yield Silence currentSilence

                            currentSound <-
                                { currentSound with
                                    Start = sample.TimeStamp
                                    Stop = reader.TotalTime }
                    | true when sample = finalSample ->

                        let snd =
                            { currentSound with
                                Stop = currentSilence.Start
                                Length = currentSilence.Start - currentSound.Start }

                        let snc =
                            { currentSilence with
                                Length = currentSilence.Stop - currentSilence.Start }

                        if snc.Length < maxSilenceDuration then
                            yield
                                Sound
                                    { snd with
                                        Stop = reader.TotalTime
                                        Length = reader.TotalTime - snd.Start }
                        else
                            yield Sound snd
                            yield Silence snc
                    | false when sample = finalSample ->
                        yield
                            Sound
                                { currentSound with
                                    Stop = sample.TimeStamp
                                    Length = currentSilence.Start - currentSound.Start }                    
                    | _ as isSilence -> ()                                
            }
            |> List.ofSeq

        if isNotNullorEmpty opts.OutputFile then
            let newDuration =
                overwriteSilences reader opts.OutputFile opts.MaxSilenceDuration ranges false

            if opts.List then
                report ranges newDuration reader.TotalTime opts

        else if opts.List then
            report ranges TimeSpan.Zero reader.TotalTime opts

        if isNotNullorEmpty opts.MarkPath then
            markSilences opts.MarkPath opts.MaxSilenceDuration reader ranges |> ignore

    [<EntryPoint>]
    let main argv =

        let result = CommandLine.Parser.Default.ParseArguments<DetectOptions>(argv)

        match result with
        | :? Parsed<DetectOptions> as parsed -> DetectSilence parsed.Value
        | :? NotParsed<DetectOptions> as notParsed -> failwithf "-X- %A" notParsed.Errors
        | _ -> failwith $"{result.GetType().FullName} is not supported"

        0
