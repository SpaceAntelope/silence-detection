namespace SilenceDetect

module Reporters = 
    open System
    open Common
    
    let report (intervals: VolumeInterval list) (outputDurationFromWriter: TimeSpan) (originalDuration: TimeSpan) (opts: DetectOptions) =

        let maxSilenceSpan = TimeSpan.FromSeconds(opts.MaxSilenceDuration)

        let silenceCount =
            intervals
            |> List.filter (function
                | Silence x when x.Length >= maxSilenceSpan -> true
                | _ -> false)
            |> List.length

        let (silenceDuration, soundDuration) =
            intervals
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
           ActualOutputDuration = outputDurationFromWriter
           OutputFile =
            if isNotNullorEmpty opts.OutputFile then
                IO.Path.GetFullPath(opts.OutputFile)
            else
                ""

           Intervals =
            intervals
            |> List.map (function
                | Silence x -> {| x with Case = "Silence" |}
                | Sound x -> {| x with Case = "Sound" |}) |}
        |> toJson
        |> printfn "%s"

