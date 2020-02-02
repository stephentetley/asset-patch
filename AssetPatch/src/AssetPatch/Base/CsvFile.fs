// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.Base

module CsvFile = 

    open FSharp.Data

    
    /// Joins with Environment.NewLine
    let private fromLines (source:string list) : string = 
        String.concat System.Environment.NewLine source
    
    /// Makes a dummy row for a CsvFile.
    /// We can't remove the dummy row at this point because `csv.Skip 1` 
    /// changes the type to CsvFile<CsvRow>.
    let private makeDummyRow (columnCount:int) (separator:string) : string = 
        List.replicate columnCount "abc" |> String.concat separator
    
    
    /// Make a CsvFile with headers and a dummy row.
    /// We can't remove the dummy row at this point because `csv.Skip 1` 
    /// changes the type to CsvFile<CsvRow>.
    let private makeDummyHeaders (headers:string[]) (sep:char) (quote:char) : CsvFile = 
        let separators = sep.ToString()
        let headings = String.concat separators headers
        let dummy = makeDummyRow headers.Length separators
        let body = fromLines [ headings; dummy ]
        CsvFile.Parse(text = body, hasHeaders = true, quote = quote, separators = separators)
    

    type CsvOptions = 
        { Separator: char
          Quote: char
        }
    
    let csvDefaults : CsvOptions = 
        { Separator = ','
          Quote = '"'
        }

    let writeCsv (headers : string []) (csvOptions: CsvOptions) (rows: seq<string []>) (outputPath: string) : unit = 
        let makeCsvRow (parent:CsvFile) (row:string []) : CsvRow = new CsvRow(parent=parent, columns = row)
        let parent:CsvFile = makeDummyHeaders headers csvOptions.Separator csvOptions.Quote
        let rows:seq<CsvRow> = rows |> Seq.map (makeCsvRow parent)
        let csv1 = parent.Skip 1
        let csv2 = csv1.Append rows
        csv2.Save(path = outputPath, separator = csvOptions.Separator, quote = csvOptions.Quote)


    