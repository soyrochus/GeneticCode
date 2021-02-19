namespace GenCode

open System.Linq

module Encoder =
    open System
    type Codon = DataCodon of byte | StopCodon of byte
    type InvalidStates =
        | UnexpectedEnd of char[]
        | InvalidCharacters of char []
    
    let A = 0uy ; 
    let U = 1uy  // 01
    let G = 2uy  // 10
    let C = 3uy  // 11
    let RNA = [|'A'; 'U'; 'G'; 'C'|]
    let stop0 = StopCodon 49uy //Codon where instance of Codon = UAG 110001 | UGA  001110 | UAA 000010
    let stop1 = StopCodon 14uy
    let stop2 = StopCodon 02uy
    
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
    
       
    let inline new_state chr crumb state =        
        DataCodon (chr * (byte (4. ** float crumb)) ||| state)
    
   (* let swap_if_stop_codon(codon) =
        match codon with
         | (DataCodon c) when c = stop0 || c = stop0 || c = stop0 -> StopCodon c         
         | _ -> codon
   *)          
    let encode_codon(chrs: char []): Codon =
        let mutable count = 0 
        Array.fold (fun (codon: Codon) chr -> let crumb = count  // A pair of two bits or a quarter byte was called a crumb, often used in early 8-bit computing (Atari 2600, ZX Spectrum). It is now largely defunct.
                                              count <- count + 1
                                              let state = match codon with
                                                                        | DataCodon s -> s                                                                         
                                                                        | _ -> failwith "Not a valid Codon "
                                              match chr with
                                                | 'A' -> new_state A crumb state  
                                                | 'U' -> new_state U crumb state 
                                                | 'G' -> new_state G crumb state
                                                | 'C' -> new_state C crumb state
                                                | _ -> failwith "Invalid nucleotide for ") (DataCodon 0uy) chrs
    
    let inline get_crumb crumb (codon: byte) =
        let cnum = 3 * int (4. ** float crumb) &&& int codon 
        RNA.[cnum >>> (crumb * 2)]
        
    let decode_codon(codon: Codon ): char [] =
        match codon with
         | DataCodon c -> [| get_crumb 0 c ; get_crumb 1 c; get_crumb 2 c|]
         | _ -> failwith "Invalid Codon"
        
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
                
                decode_codon (DataCodon 48uy) |> should equal [|'A'; 'A'; 'C'|] // 100000 CAA
                decode_codon (DataCodon 3uy)  |> should equal [|'C'; 'A'; 'A'|]   // 000010 AAC
                decode_codon (DataCodon 45uy) |> should equal [|'U'; 'C'; 'G'|] // 101101 GCU
                
                              
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
        
        