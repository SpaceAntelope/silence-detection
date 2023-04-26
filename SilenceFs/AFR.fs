namespace SilenceDetect

[<RequireQualifiedAccess>]
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
        let buffer = Array.zeroCreate<single> (samplesPerSecond)

        if start.TotalMilliseconds > 0 then
            reader.Seek(int64 (float samplesPerSecond * start.TotalSeconds), SeekOrigin.Begin)
            |> printfn "reader seek result: %A"

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

    let TakeSamples (f: single seq -> unit) (samplesToCompletion: int) (reader: AudioFileReader) =
        let samplesToCompletion = samplesToCompletion |> alignToBlock reader
        let samplesPerSecond = Math.Min(sps reader, samplesToCompletion)

        let buffer = Array.zeroCreate<single> (samplesPerSecond)

        seq {
            let mutable sampleCount = 1
            let mutable samplesProcessed = 0

            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                let remaining = samplesToCompletion - samplesProcessed |> alignToBlock reader
                let required = Math.Min(buffer.Length, remaining)
                sampleCount <- reader.Read(buffer, 0, required)
                samplesProcessed <- samplesProcessed + sampleCount
                yield! (buffer |> Array.take sampleCount)
        }
        |> f

        reader

    let mapSamples (sampleCount: int) (reader: AudioFileReader) =
        let samplesToCompletion = sampleCount |> alignToBlock reader
        let samplesPerSecond = Math.Min(sps reader, samplesToCompletion)

        let buffer = Array.zeroCreate<single> (samplesPerSecond)

        seq {
            let mutable sampleCount = 1
            let mutable samplesProcessed = 0

            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                let remaining = samplesToCompletion - samplesProcessed |> alignToBlock reader
                let required = Math.Min(buffer.Length, remaining)
                sampleCount <- reader.Read(buffer, 0, required)
                samplesProcessed <- samplesProcessed + sampleCount
                yield! (buffer |> Array.take sampleCount)
        }



    let TakeBytes (f: byte seq -> unit) (samplesToCompletion: int) (reader: AudioFileReader) =
        let b2s = int byte2singleConversionFactor
        let bytesToCompletion = samplesToCompletion |> alignToBlock reader

        let bytesPerSecond = Math.Min(sps reader * b2s, bytesToCompletion) // in case of very short required sample count

        // printfn "%d %d" samplesToCompletion samplesPerSecond

        let buffer = Array.zeroCreate<byte> (bytesPerSecond)

        seq {
            let mutable sampleCount = 1
            let mutable samplesProcessed = 0

            while sampleCount > 0 && samplesProcessed < samplesToCompletion do
                let remaining = samplesToCompletion - samplesProcessed |> alignToBlock reader
                let required = Math.Min(buffer.Length, remaining)
                sampleCount <- reader.Read(buffer, 0, required)
                samplesProcessed <- samplesProcessed + sampleCount
                yield! (buffer |> Array.take sampleCount)

            if (samplesToCompletion - samplesProcessed) > reader.WaveFormat.BlockAlign then
                failwith
                    $"Track synchronization is off: Requested samples: {samplesToCompletion} Processed samples: {samplesProcessed}"
        }
        |> f

        reader

    let SkipSamples (sampleCount: int64) (reader: AudioFileReader) =
        let byte2singleFactor = int64 (sizeof<float32> / sizeof<byte>)
        printf $"Skip {reader.CurrentTime}"
        reader.Seek(sampleCount * byte2singleFactor, SeekOrigin.Current) |> ignore
        printfn $" -> {reader.CurrentTime} ({TimeSpan.FromSeconds(float sampleCount / 44100.0)})"

        reader
