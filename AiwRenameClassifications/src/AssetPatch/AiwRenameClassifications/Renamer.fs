namespace AssetPatch.AiwRenameClassifications

module Renamer =

    open System.IO
    open System         // String.IsNullOrWhiteSpace

    open AssetPatch.Base
    open AssetPatch.Base.Aiw.ChangeFileParser

    type FindReplacePair = 
        { FindText: string 
          ReplaceText: string
        }

    // ("FUNCLOC", "$9921"); ("FLOC_REF", "WRE01-CAA"); ("STATTEXT", "CRTE");
    // ("USTW_FLOC", "UCON")
    let private readFlocStaginginLine(row: AssocList<string, string>) : Option<FindReplacePair> = 
        match AssocList.tryFind4 "FUNCLOC" "FLOC_REF" "STATTEXT" "USTW_FLOC" row with
        | None -> None
        | Some (flocId, flocRef, status, userStatus) -> 
            if flocId.StartsWith("$") && status = "CRTE" && userStatus = "UCON" then
                Some { FindText = flocRef; ReplaceText = flocId }
            else None



    let readFlocStagingFile (path: string): Result<FindReplacePair list, string> = 
        match readAiwChangeFile path with
        | Error msg -> Error msg
        | Ok ans -> 
            ans.RowAssocs() 
                |> List.choose readFlocStaginginLine
                |> Ok

    let renameLine1(src: string) (findrep: FindReplacePair): string option = 
        if src.Contains(findrep.FindText) then
            src.Replace(oldValue = findrep.FindText, newValue = findrep.ReplaceText) |> Some
        else
            None

    let private renameLine(src: string) (findreps: FindReplacePair list): Result<string, string> = 
        let rec work xs = 
            match xs with
            | [] -> None
            | x :: rs -> 
                match renameLine1 src x with
                | Some s  -> Some s
                | None -> work rs
        match work findreps with
        | None -> Error ("findReplace failed for: " + src)
        | Some s -> Ok s

    let renameLines(src: string list) (findreps: FindReplacePair list): Result<string list, string> = 
        let rec work xs (fk : string -> Result<string list, string>) (sk: string list -> Result<string list, string>) = 
            match xs with
            | [] -> sk []
            | (x : string) :: rs -> 
                if x.StartsWith("*") || String.IsNullOrWhiteSpace(x) then 
                    work rs fk (fun vs -> sk (x :: vs))
                else 
                    match renameLine x findreps with
                    | Error msg ->  fk msg
                    | Ok str -> work rs fk (fun vs -> sk (str :: vs))
        work src (fun msg -> Error msg) (fun xs -> Ok xs)
            
    let renamePatch(path: string) (findreps: FindReplacePair list): Result<unit, string> = 
        let dir = Path.GetDirectoryName(path)
        let outfile = Path.GetFileNameWithoutExtension(path) + "_002"
        let outpath = Path.Combine(dir, outfile) |> fun x -> Path.ChangeExtension(x, "txt")
        
        try 
            let lines = File.ReadAllLines(path) |> Array.toList
            match renameLines lines findreps with
            | Error msg -> Error msg
            | Ok xs -> 
                File.WriteAllLines(path = outpath, contents = List.toArray xs)
                Ok ()
        with
        | ex -> Error ex.Message


    let renamePatches (stagingFilePath: string) (srcpaths: string list) : Result<unit, string> = 
        let rec work findreps xs fk sk = 
            match xs with
            | [] -> sk ()
            | x :: rs -> 
                match renamePatch x findreps with
                | Error msg -> fk msg
                | Ok _ -> work findreps rs fk sk

        
        match  readFlocStagingFile stagingFilePath with
        | Error msg -> Error msg
        | Ok findreps -> work findreps srcpaths Error Ok



            
            
