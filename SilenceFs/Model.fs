namespace Silence


module Model = 
    open CommandLine
    open System
        
    type DetectOptions = {
        [<Option('i', "input", Required=true, HelpText="Path to input audio file.")>]
        InputFile : string
        [<Option('o', "output", HelpText="Path to ouput audio file.")>]
        OutputFile : string
        [<Option("offset", Default=0.0, HelpText="Start processing this many seconds from start")>]
        Offset: float
        [<Option('d',"duration", HelpText="Length of excerpt in seconds")>]
        Duration: float option
        [<Option('l', "list", HelpText="List silences")>]
        List: bool
        [<Option('s', "sampling", Default =10.0, HelpText="Sample duration in milliseconds")>]
        Sampling: float    
        [<Option('m',"min-silence",Default=0.25, HelpText="Max silence duration in seconds")>]
        MaxSilenceDuration: float
        // [<Option('n',"new-silence",Default=0.25, HelpText="Minimum silence duration in seconds")>]
        // NewSilenceDuration: float
    }

    type RangeData = { 
        Start: TimeSpan
        Stop: TimeSpan
        Length: TimeSpan
    } with static member Empty = {    
            Start = TimeSpan.Zero
            Stop = TimeSpan.Zero
            Length = TimeSpan.Zero }

    type Range = 
    | Silence of RangeData
    | Sound of RangeData