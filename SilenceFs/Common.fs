namespace SilenceDetect

module Common =
    open NAudio.Wave
    open System
    open Newtonsoft.Json

    let isNullorEmpty = System.String.IsNullOrEmpty
    let isNotNullorEmpty = isNullorEmpty >> not

    let sps (reader: AudioFileReader) =
        reader.WaveFormat.SampleRate * reader.WaveFormat.Channels

    let span2samples (reader: AudioFileReader) (span: TimeSpan) =
        (span.TotalSeconds * float (sps reader)) |> int

    let span2bytes (reader: AudioFileReader) (span: TimeSpan) =
        (span.TotalSeconds * float reader.WaveFormat.AverageBytesPerSecond) |> int

    let alignToBlock (reader: AudioFileReader) (bufferLength: int) =
        bufferLength - (bufferLength % reader.BlockAlign)

    let byte2singleConversionFactor = int64 (sizeof<float32> / sizeof<byte>)

    let toJson x =
        JsonConvert.SerializeObject(x, Formatting.Indented)
