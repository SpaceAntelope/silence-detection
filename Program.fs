// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"


open System
open System.IO
open NAudio.Wave
// open WaveFormRendererLib

let drainStream (reader: WaveFileReader) =
    let buffer : byte[] = Array.zeroCreate<single> (reader.WaveFormat.SampleRate * reader.WaveFormat.Channels)
    
    seq {
        let mutable frameCount =  1
        while frameCount > 0 do
            frameCount <- reader.Read(buffer)
            yield! buffer
    } |> Array.ofSeq
    

let DetectSilence (filename:string) =
    //let samples = 
    use reader = new WaveFileReader(filename)
    let samples = drainStream reader

    let threshold = 0.01f
    let silenceDuration = TimeSpan.FromSeconds(1)
    let mutable silenceStart = TimeSpan.Zero

    for i in 0 .. samples.Length - 1 do
        if Math.Abs(samples.[i]) < threshold
        then
            if silenceStart = TimeSpan.Zero
            then silenceStart <- reader.CurrentTime
        else
            if silenceStart <> TimeSpan.Zero
            then 
                let duration = reader.CurrentTime - silenceStart
                if duration >= silenceDuration 
                then printfn $"Silence detected from {silenceStart} to {reader.CurrentTime}"
                silenceStart <- TimeSpan.Zero


[<EntryPoint>]
let main argv =
    DetectSilence argv.[0]

    0