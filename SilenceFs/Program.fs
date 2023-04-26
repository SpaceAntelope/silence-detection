namespace SilenceDetect

open System
open System.IO
open NAudio.Wave
open CommandLine
open Newtonsoft.Json
open Common

module Program =

    let DetectSilence (opts: DetectOptions) =

        if (opts.MaxSilenceDuration < 2.0 * opts.Padding) then
            failwith
                $"Total padding is bigger than requested silence duration. --padding should at most be half of --max-silence ."

        let offset = TimeSpan.FromSeconds(opts.Offset)
        let duration = opts.Duration |> Option.map (fun dur -> TimeSpan.FromSeconds(dur))

        let addPadding (intervals: VolumeInterval list) =
            let padDuration = TimeSpan.FromSeconds(opts.Padding)

            if padDuration.TotalSeconds > 0 then
                intervals |> VolumeInterval.padSound padDuration
            else
                intervals

        use reader = new AudioFileReader(opts.InputFile)

        let samplesPerSecond = sps reader
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
                   Sample = avg |})
            |> List.ofSeq


        let intervals =
            let mutable silence = false
            let volumeTreshold = 0.0075
            let maxSilenceDuration = TimeSpan.FromSeconds(opts.MaxSilenceDuration)

            let mutable currentSilence =
                { VolumeInterval.empty with
                    Start = offset }

            let mutable currentSound =
                { VolumeInterval.empty with
                    Start = offset }

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
            |> addPadding

        if isNotNullorEmpty opts.OutputFile then
            let newDuration =
                VolumeInterval.writeFileWithSilenceCropped
                    opts.OutputFile
                    opts.MaxSilenceDuration
                    false
                    intervals
                    reader

            if opts.List then
                Reporters.report intervals newDuration reader.TotalTime opts

        else if opts.List then
            Reporters.report intervals TimeSpan.Zero reader.TotalTime opts

        if isNotNullorEmpty opts.MarkPath then
            VolumeInterval.createFileWithSilenceMarked opts.MarkPath reader intervals
            |> ignore


    [<EntryPoint>]
    let main argv =

        let result = CommandLine.Parser.Default.ParseArguments<DetectOptions>(argv)

        match result with
        | :? Parsed<DetectOptions> as parsed -> DetectSilence parsed.Value
        | :? NotParsed<DetectOptions> as notParsed -> failwithf "-X- %A" notParsed.Errors
        | _ -> failwith $"{result.GetType().FullName} is not supported"

        0
