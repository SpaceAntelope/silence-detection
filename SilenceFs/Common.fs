namespace Silence 

module Common = 
    open NAudio.Wave
    let isNullorEmpty = System.String.IsNullOrEmpty
    let isNotNullorEmpty = isNullorEmpty >> not
    let sps (reader: AudioFileReader) =
        reader.WaveFormat.SampleRate * reader.WaveFormat.Channels

    let byte2singleConversionFactor = int64 (sizeof<float32>/sizeof<byte>)