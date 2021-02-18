namespace GenCode

open System.Linq

module Encoder =
    open System
    type Codon = DataCodon of byte | StopCodon of byte
    type InvalidStates =
        | UnexpectedEnd of char[]
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
    
    let A = 0uy  // 00
    let U = 1uy  // 01
    let G = 2uy  // 10
    let C = 3uy  // 11
         
    let inline new_state (chr: byte) (nibble: float) (state: byte) =        
        DataCodon (chr * (byte (4. ** nibble)) ||| state)
         
    let encode_codon(chrs: char []): Codon =
        let mutable count = 0. 
        Array.fold (fun (codon: Codon) chr -> let nibble = count
                                              count <- count + 1.
                                              let state = match codon with
                                                                        | DataCodon s -> s
                                                                        | _ -> failwith "Not a valid Codon "
                                              match chr with
                                                | 'A' -> new_state A nibble state  
                                                | 'U' -> new_state U nibble state 
                                                | 'G' -> new_state G nibble state
                                                | 'C' -> new_state C nibble state
                                                | _ -> failwith "Invalid nucleotide for ") (DataCodon 0uy) chrs
               
    let chars_to_codon (chrs : char []) : Result<Codon, InvalidStates>  =
                         
        if Array.length chrs < 3 then
            Error (UnexpectedEnd chrs)
        elif not (valid_rna_codon chrs) then
            Error (InvalidCharacters chrs)
        else    
            Ok (DataCodon 0uy)
               
    let tokens e =
        Seq.filter (skip_comment ()) e |> Seq.filter (fun c -> not(Char.IsWhiteSpace(c))) 
    
    let codons (s: seq<char>) : Result<Codon, InvalidStates> seq =        
        s |> Seq.map Char.ToUpper |> Seq.chunkBySize 3 |> Seq.map chars_to_codon   
                                                        
    module Test =     
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Testing encoding and decoding of Coding in/from a single byte `` () =
                encode_codon [|'A'; 'A'; 'C'|] |> should equal (DataCodon 48uy) // 100000 CAA
                encode_codon [|'C'; 'A'; 'A'|] |> should equal (DataCodon 3uy)  // 000010 AAC
                encode_codon [|'U'; 'C'; 'G'|] |> should equal (DataCodon 45uy) // 101101 GCU
                
                
                              
        [<Test>]
        let ``Testing whether array of chars represents valid RNA codon (triplet)`` () =
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
        
        