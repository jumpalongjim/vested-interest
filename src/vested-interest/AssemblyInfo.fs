namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("vested-interest")>]
[<assembly: AssemblyProductAttribute("vested-interest")>]
[<assembly: AssemblyDescriptionAttribute("Record investment activity and track results")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
