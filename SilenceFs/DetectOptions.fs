namespace SilenceDetect

open CommandLine

type DetectOptions =
    { [<Option('i', "input", Required = true, HelpText = "Path to input audio file.")>]
      InputFile: string
      [<Option('o', "output", HelpText = "Path to ouput audio file.")>]
      OutputFile: string
      [<Option("offset", Default = 0.0, HelpText = "Start processing this many seconds from start")>]
      Offset: float
      [<Option('d', "duration", HelpText = "Length of excerpt in seconds")>]
      Duration: float option
      [<Option('l', "list", HelpText = "List silences")>]
      List: bool
      [<Option('s', "sampling", Default = 10.0, HelpText = "Sample duration in milliseconds")>]
      Sampling: float
      [<Option('m', "max-silence", Default = 0.25, HelpText = "Max silence duration in seconds")>]
      MaxSilenceDuration: float
      // [<Option('n',"new-silence",Default=0.25, HelpText="Minimum silence duration in seconds")>]
      // NewSilenceDuration: float
      [<Option("mark", HelpText = "Mark silences on sound graph")>]
      MarkPath: string
      [<Option('p',
               "padding",
               Default = 0.1,
               HelpText =
                   "Padding in seconds arround non-silent parts to preserve voice rising or dropping. Duration is included in --max-silence length, so it should last more than half the requested silence, for obvious reasons.")>]
      Padding: float }
