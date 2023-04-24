namespace Silence


open System
open System.IO
open NAudio.Wave
open CommandLine
open Model
open Newtonsoft.Json


module Program = 
    let isNullorEmpty = String.IsNullOrEmpty
    let isNotNullorEmpty = isNullorEmpty>>not

    let report (ranges: Range list ) (duration: TimeSpan) (originalDuration: TimeSpan) (opts: DetectOptions)=
        
        let maxSilenceSpan = TimeSpan.FromSeconds(opts.MaxSilenceDuration)
        let silenceCount = 
            ranges 
            |> List.filter (
                function 
                | Silence x when x.Length >= maxSilenceSpan  -> true 
                | _ -> false) 
            |> List.length

        let (silenceDuration, soundDuration) = 
            ranges
            |> List.fold (fun (silence,sound) current -> 
                match current with
                | Silence x -> (silence + x.Length, sound)
                | Sound x -> (silence, sound + x.Length)
            ) (TimeSpan.Zero, TimeSpan.Zero)
        
        {|
            SilenceDuration = silenceDuration
            SoundDuration = soundDuration
            ExpectedInputDuration = silenceDuration + soundDuration
            ActualInputDuration = originalDuration
            ExpectedOutputDuration = soundDuration + float silenceCount * TimeSpan.FromSeconds(opts.MaxSilenceDuration)
            ActualOutputDuration = duration
            OutputFile = IO.Path.GetFullPath(opts.OutputFile)

            Ranges = ranges |> List.map (
                function 
                | Silence x -> {| x with Case = "Silence" |} 
                | Sound x -> {|x with Case = "Sound"|})
        |} 
        |> fun x -> JsonConvert.SerializeObject(x, Formatting.Indented)
        |> printfn "%s"

    let DetectSilence (opts: DetectOptions) =  
        //printfn "%A" opts
        let offset = TimeSpan.FromSeconds(opts.Offset)
        let duration = opts.Duration |> Option.map (fun dur -> TimeSpan.FromSeconds(dur))

        use reader = new AudioFileReader(opts.InputFile)
        
        let samplesPerSecond = reader.WaveFormat.SampleRate * reader.WaveFormat.Channels;
        let bin = TimeSpan.FromMilliseconds(opts.Sampling)
        let samplesPerBin = float samplesPerSecond * bin.TotalSeconds
        
        // printfn $"Samples/sec: {samplesPerSecond}"

        let indexedBinnedSamples = 
            reader
            |> AFR.Drain offset duration
            |> Seq.chunkBySize (int samplesPerBin)
            |> Seq.map(Array.map(Math.Abs)>>Array.average>>float)    
            |> Seq.mapi(fun i avg ->
                    {|  TimeStamp = TimeSpan.FromMilliseconds(float i * bin.TotalMilliseconds + offset.TotalMilliseconds + opts.Sampling)
                        Sample = Math.Round(avg * 1e4, 3) |})
        
        let mutable silence = false
        let volumeTrehshold = 75.0
        let minSilenceDuration = TimeSpan.FromSeconds(opts.MaxSilenceDuration)

        let mutable currentSilence = { RangeData.Empty with Start = offset }
        let mutable currentSound =  { RangeData.Empty with Start = offset }

        let ranges = 
            seq {    
                for sample in indexedBinnedSamples do
                    
                    match sample.Sample <= volumeTrehshold with                
                    | true when silence = false ->
                        silence <- true
                        currentSilence <- { currentSilence with Start = sample.TimeStamp; Stop = reader.TotalTime } //; Samples = ResizeArray() }
                        
                    | false when silence = true -> 
                        silence <- false 
                        currentSilence <- { currentSilence with Stop = sample.TimeStamp; Length = sample.TimeStamp - currentSilence.Start }
                        
                        if currentSilence.Length >= minSilenceDuration 
                        then
                            yield Sound { currentSound with Stop = currentSilence.Start; Length = currentSilence.Start - currentSound.Start } 
                            yield Silence currentSilence
                            currentSound <- { currentSound with Start = sample.TimeStamp; Stop = reader.TotalTime }                                    
                        
                    | true -> ()
                    | false -> ()
            } |> List.ofSeq
        

        if isNotNullorEmpty opts.OutputFile
        then            
            let span2samples (span: TimeSpan) =
                span.TotalSeconds * float samplesPerSecond |> int

            let shortSilence = Array.create  (int(opts.MaxSilenceDuration * float samplesPerSecond)) (0.0f)

            reader.Position <- 0
            use writer = new WaveFileWriter(opts.OutputFile, WaveFormat(44100, 24, 2))
            
            List.fold (fun state current -> 
                match current with
                | Silence range ->                 
                    //let count = range.Length |> span2samples
                    reader.CurrentTime <- reader.CurrentTime.Add(range.Length)
                    // reader |> AFR.SkipSamples count           
                    reader
                | Sound range -> 
                    let count = range.Length |> span2samples
                    reader 
                    |> AFR.TakeSamples (fun buff -> 
                        let samples = buff |> Array.ofSeq
                        writer.WriteSamples(samples, 0, samples.Length)
                        writer.WriteSamples(shortSilence, 0, shortSilence.Length)                        
                    ) count
            ) reader ranges |> ignore            
            writer.Flush()

            if opts.List then report ranges writer.TotalTime reader.TotalTime opts                  
            
    [<EntryPoint>]
    let main argv =

        let result = CommandLine.Parser.Default.ParseArguments<DetectOptions>(argv)
        match result with
        | :? Parsed<DetectOptions> as parsed -> DetectSilence parsed.Value
        | :? NotParsed<DetectOptions> as notParsed -> failwithf "-X- %A" notParsed.Errors
        | _ -> failwith $"{result.GetType().FullName} is not supported"
        
        0    


