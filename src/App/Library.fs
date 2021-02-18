namespace GenCode

open System.Linq

module Encoder =
    open System
    type Codon = DataCodon of int | StopCodon of int
    type InvalidStates =
        | UnexpectedEnd of int
        | InvalidCharacters of char []
        
    let rec skip_comment () : (Char) -> bool =
        let mutable skip = false
        fun (c:Char) ->
            if c = '>' then
                skip <- true
                false
            elif skip && c='\n' then
                skip <- false
                false
            elif skip then
                false
            else
                true
    
    //let encode_codon (chrs : char []) : Result<Codon, InvalidStates> =
    let valid_rna_codon (chrs : char []) =
        Array.forall (fun c -> c = 'A' || c = 'U' || c = 'G'|| c = 'C') chrs
        
    let chars_to_codon (chrs : char []) : Result<Codon, InvalidStates>  =
        
        let l = Array.length chrs        
        if l < 3 then
            Error (UnexpectedEnd l)
        elif not (valid_rna_codon chrs) then
            Error (InvalidCharacters chrs)
        else    
            Ok (DataCodon 0)
           
    let tokens e =
        Seq.filter (skip_comment ()) e |> Seq.filter (fun c -> not(Char.IsWhiteSpace(c))) 
    
    let codons (s: seq<char>) : Result<Codon, InvalidStates> seq =        
        s |> Seq.map Char.ToUpper |> Seq.chunkBySize 3 |> Seq.map chars_to_codon   
                                                        
    module Test =     
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Testing whether array of chars represent validity of RNA `` () =
            valid_rna_codon [|'A';'U'; 'G'|] |> should equal true
            valid_rna_codon [|'G';'C'; 'A'|] |> should equal true
            valid_rna_codon [|'T';'U'; 'G'|] |> should equal false //DNA
        
        [<Test>]
        let ``Retrieve tokens from sequence, ignoring whitespace and comments`` () = 
            let t = tokens "c G\na\n>BLAB ALA\nU"
                        
            let e = t.GetEnumerator()
            e.MoveNext() |> ignore            
            e.Current |> should equal 'c'
            e.MoveNext() |> ignore            
            e.Current |> should equal 'G'
            e.MoveNext() |> ignore            
            e.Current |> should equal 'a'
            e.MoveNext() |> ignore                                    
            e.Current |> should equal 'U'
            e.MoveNext() |> should equal false
        
        