namespace Silence

module AFR = 
    open System
    open NAudio.Wave
    open System.IO
    open Common

    let Take (duration: TimeSpan) (reader: AudioFileReader) =
        WaveExtensionMethods.Take(reader, duration) :?> AudioFileReader
    let Skip (duration: TimeSpan) (reader: AudioFileReader) =
        WaveExtensionMethods.Skip(reader, duration) :?> AudioFileReader
    
    let Drain (start: TimeSpan) (duration: TimeSpan option) (reader: AudioFileReader) =
        let samplesPerSecond = reader.WaveFormat.SampleRate * reader.WaveFormat.Channels
        let buffer  = Array.zeroCreate<single> (samplesPerSecond)
        
        if start.TotalMilliseconds > 0 
        then reader.Seek(int64(float samplesPerSecond * start.TotalSeconds), SeekOrigin.Begin) |> printfn "reader seek result: %A"

        let stopDuration = duration |> Option.defaultValue reader.TotalTime
        let samplesToCompletion = stopDuration.TotalSeconds * (float samplesPerSecond)

        seq {
            let mutable sampleCount = reader.Length
            let mutable samplesProcessed = 0.0
            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                sampleCount <- reader.Read(buffer, 0, buffer.Length)
                samplesProcessed <- samplesProcessed + float sampleCount
                yield! buffer
        }

    let TakeSamples (f: single seq -> unit) (samplesToCompletion : int) (reader: AudioFileReader) =
        // printf "%d %d " reader.WaveFormat.BlockAlign samplesToCompletion
        let samplesToCompletion = samplesToCompletion - (samplesToCompletion % reader.WaveFormat.BlockAlign) // round to block align

        let samplesPerSecond = Math.Min(sps reader, samplesToCompletion)
        
        // printfn "%d %d" samplesToCompletion samplesPerSecond
        
        let buffer  = Array.zeroCreate<single> (samplesPerSecond)       
        
        seq {
            let mutable sampleCount =  1
            let mutable samplesProcessed = 0
            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                sampleCount <- reader.Read(buffer, 0, buffer.Length)
                samplesProcessed <- samplesProcessed + sampleCount
                yield! (buffer |> Array.take sampleCount)
        } |> f

        reader

    let TakeBytes (f: byte seq -> unit) (samplesToCompletion: int) (reader: AudioFileReader) =
        let b2s = int byte2singleConversionFactor
        let bytesToCompletion = b2s * samplesToCompletion
        let bytesToCompletion = samplesToCompletion - (samplesToCompletion % reader.WaveFormat.BlockAlign) // round to block align

        let bytesPerSecond = Math.Min(sps reader * b2s, bytesToCompletion)
        
        // printfn "%d %d" samplesToCompletion samplesPerSecond
        
        let buffer  = Array.zeroCreate<byte> (bytesPerSecond)
        
        seq {
            let mutable sampleCount =  1
            let mutable samplesProcessed = 0
            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                sampleCount <- reader.Read(buffer, 0, buffer.Length)
                samplesProcessed <- samplesProcessed + sampleCount
                yield! (buffer |> Array.take sampleCount)
        } |> f

        reader

    let SkipSamples (sampleCount : int64) (reader: AudioFileReader) =
        let byte2singleFactor = int64 (sizeof<float32>/sizeof<byte>)
        printf $"Skip {reader.CurrentTime}"
        reader.Seek(sampleCount * byte2singleFactor, SeekOrigin.Current) |> ignore
        printfn $" -> {reader.CurrentTime} ({TimeSpan.FromSeconds(float sampleCount/ 44100.0)})"
        
        reader

    
    