namespace Silence


open System
open System.IO
open NAudio.Wave
open CommandLine
open Model


module Program = 
    let isNullorEmpty = String.IsNullOrEmpty
    let isNotNullorEmpty = isNullorEmpty>>not

    let DetectSilence (opts: DetectOptions) =  
        printfn "%A" opts
        let offset = TimeSpan.FromSeconds(opts.Offset)
        let duration = opts.Duration |> Option.map (fun dur -> TimeSpan.FromSeconds(dur))

        use reader = new AudioFileReader(opts.InputFile)
        
        let samplesPerSecond = reader.WaveFormat.SampleRate * reader.WaveFormat.Channels;
        let bin = TimeSpan.FromMilliseconds(opts.Sampling)
        let samplesPerBin = float samplesPerSecond * bin.TotalSeconds
        
        printfn $"Samples/sec: {samplesPerSecond}"

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

        
        if opts.List then ranges |> List.iter (printfn "%A")

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

            printfn $"Old duration: {ranges |> List.map (fun x-> match x with Silence y -> y.Length.TotalSeconds | Sound y -> y.Length.TotalSeconds) |> List.sum |> TimeSpan.FromSeconds}"
            printfn $"New duration: {writer.TotalTime}"

            printfn $"Output file written at {IO.Path.GetFullPath(opts.OutputFile)}"
            
    [<EntryPoint>]
    let main argv =

        let result = CommandLine.Parser.Default.ParseArguments<DetectOptions>(argv)
        match result with
        | :? Parsed<DetectOptions> as parsed -> DetectSilence parsed.Value
        | :? NotParsed<DetectOptions> as notParsed -> failwithf "-X- %A" notParsed.Errors
        | _ -> failwith $"{result.GetType().FullName} is not supported"
        
        0    


