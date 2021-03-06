(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"

#r "System.Drawing"

open System.Drawing

//Size : HxW
//Render 144x160

open Fable.Core
open Fable.Import.Browser

[<Emit("Math.random()")>]
let random (): float = jsNative

module String = 
    [<Emit("""$1.charAt($0)""")>]
    let char (i:int) (s:string) : char = failwith "not yet implemented"

module Array2D =
    [<Emit("""$0""")>]
    let ofArray (l: 'a [][]) : 'a [,] = failwith "not yet implemented"
 
    [<Emit("""$0[0].length""")>]
    let length2 (x: _ [,]) = failwith "not yet implemented"

    [<Emit("""$0.length""")>]
    let length1 (x: _ [,]) = failwith "not yet implemented"

    [<Emit("""
    for(var y=0; y < $1.length; y++) {
        for(var x=0; x < $1[0].length; x++) {
            var v = $1[y][x];
            $0(y)(x)(v);
        }
    }
    """)>]
    let iteri (f:int -> int -> 'a -> unit) (x: _ [,]) : unit = failwith "not yet implemented"

type Pixel<'a> = Pixel of 'a
type Line<'a> = Line of 'a Pixel seq
type Digit<'a> = Digit of 'a Line seq

module Keyboard =
  /// Set of currently pressed keys
  let mutable keysPressed = Set.empty
  /// Update the keys as requested
  let reset () = keysPressed <- Set.empty
  let isPressed keyCode = Set.contains keyCode keysPressed
  /// Triggered when key is pressed/released
  let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    let op =  if pressed then Set.add else Set.remove
    keysPressed <- op keyCode keysPressed
    null
  /// Register DOM event handlers
  let init () =
    window.addEventListener_keydown(fun e -> update(e, true))
    window.addEventListener_keyup(fun e -> update(e, false))

type Color = { Red:float; Green:float; Blue:float; Alpha:float }

module Html = 

    let drawImage (context:CanvasRenderingContext2D) x y img = context.drawImage(U3.Case1 img, float x, float y)
    
    let drawImages (context:CanvasRenderingContext2D) size x y imgs  = 
        imgs
        |> Seq.rev
        |> Seq.iteri(fun i img -> 
            let offset = if i = 0 then 0 else size * i
            img |> drawImage context (x - offset) y)

    let putImageData (context:CanvasRenderingContext2D) x y imgdata = context.putImageData(imgdata, float x, float y)

    let createImage data =
        let img = document.createElement_img()
        img.src <- data
        img

    let createBrush (context:CanvasRenderingContext2D) size color =
        let id = context.createImageData(U2.Case1 (float size), (float size))
        let d = id.data
        let m = size * size * 4
        [ 0 .. 4 .. (m - 1) ]
        |> List.iter(fun i ->
            let i' = int i
            d.[i'] <- color.Red
            d.[i' + 1] <- color.Green
            d.[i' + 2] <- color.Blue
            d.[i' + 3] <- color.Alpha)
        id

    [<Emit("alert($0);")>]
    let alert (m:'a) : unit = failwith "not yet implemented"

    let createPixels (context:CanvasRenderingContext2D) colors = 
        let xL = colors |> Array2D.length2
        let yL = colors |> Array2D.length1
        let id = context.createImageData(U2.Case1 (float xL), (float yL))
        let d = id.data
        colors
        |> Array2D.iteri(fun y x c ->
            let offset = y * xL * 4 + x * 4
//            alert(sprintf "o = %i (%i * %i * 4 + %i * 4)" offset y xL x)
            d.[offset] <- c.Red
            d.[offset + 1] <- c.Green
            d.[offset + 2] <- c.Blue
            d.[offset + 3] <- c.Alpha)
        id

module Gameboy = 
    let width = 160
    let height = 144
    let white = { Red=196.; Green=207.; Blue=161.; Alpha=255. }
    let black = { Red=65.; Green=65.; Blue=65.; Alpha=255. }
    let grey = { Red=107.; Green=115.; Blue=83.; Alpha=255. }
    let darkgrey = { Red=139.; Green=149.; Blue=109.; Alpha=255. }
    
    type AsciiArt = 
        | AsciiArt of string
        with 
            static member map f (AsciiArt a) = 
                a.Split([|"\n";"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map(fun l -> [| 0 .. (l.Length - 1) |] |> Array.map(fun i -> l |> String.char i |> f))
                |> Array2D.ofArray

    let toColor c = 
        match c with
        | '#' -> black
        | ' ' -> white
        | '/' -> darkgrey
        | '|' -> grey
        | other -> failwithf "unexpected char %c" other

    let toColors = AsciiArt.map toColor

    module Tetris = 
        let background = """data:image/png;base64,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""" 

        type Level = Level of int
        type Score = Score of int
        type Lines = Lines of int
        type Block = 
            | White
            | Square
            | TopBar
            | MiddleBar
            | BottomBar
            | T
            | RightL
            | LeftL
            | RightS
            | LeftS

        type Command = 
            | RotateLeft
            | RotateRight
            | Down

        type CurrentTetromino = 
            | CurrentTetromino of Block [,]
//            with static member next () = failwith "todo"
        
        type Land = 
            | Land of Block [,]
//            with static member empty = Array2D.create 18 10 White |> Land

        type GameState = 
            { Score:Score
              Level:Level
              Lines:Lines }
        
//        let move command game = 
//            let (CurrentTetromino t) = game.Tetromino
//
//            match command with
//            | RotateRight -> 
//                failwith "todo"
//            | RotateLeft -> 
//                failwith "todo"
//            | Down -> 
//                failwith "todo"
//            game.Tetromino

        type Tetromino = Tetromino of Block [,]

        type Turn = Computer | Player of string

//        let update next game command = 
//            let ngame = move command game
//            //If under the limit, merge and generate next currentTetromino
//            ngame

        let tetrominoes = 
            let bar = 
                [| [|TopBar;MiddleBar;MiddleBar;BottomBar|] |] |> Array2D.ofArray |> Tetromino
                
            let square = 
                [| [|Square;Square|]
                   [|Square;Square|] |] |> Array2D.ofArray |> Tetromino

            let t = 
                [| [|    T;T;T    |]
                   [|White;T;White|] |] |> Array2D.ofArray |> Tetromino

            let rightL = 
                [| [|RightL;RightL;RightL;RightL|]
                   [|RightL;White; White; White|] |] |> Array2D.ofArray |> Tetromino
            
            let leftL = 
                [| [|LeftL;LeftL;LeftL;LeftL|]
                   [|White;White;White;LeftL|] |] |> Array2D.ofArray |> Tetromino

            let rightS = 
                [| [|White; RightS;RightS|]
                   [|RightS;RightS;White |] |] |> Array2D.ofArray |> Tetromino

            let leftS = 
                [| [|LeftS;LeftS;White|]
                   [|White;LeftS;LeftS|] |] |> Array2D.ofArray |> Tetromino

            [ bar; square; t; rightL; leftL; rightS; leftS ]
            
        let asciiArts = 
            Map.ofArray [|(White, """
    
    
    
    
    
    
    
    """ |> AsciiArt |> toColors);
              (Square, """
########
#      #
# #### #
# #### #
# #### #
# #### #
#      #
########""" |> AsciiArt |> toColors);
 
              (TopBar, """
########
#///|///
#/|///|/
#///////
#|//|/|/
#///////
#/|//|//
########""" |> AsciiArt |> toColors);

              (MiddleBar, """
########
/|///|//
///|///|
|////|//
//|/////
//////|/
|/|/|///
########""" |> AsciiArt |> toColors);

              (BottomBar, """
########
//|/|//#
//////|#
/|/|///#
/////|/#
//|////#
|///|//#
########""" |> AsciiArt |> toColors);
 
              (T, """
########
#//////#
#/   #/#
#/ //#/#
#/ //#/#
#/ ###/#
#//////#
########""" |> AsciiArt |> toColors);

              (RightL, """
########
#||||||#
#||||||#
#||||||#
#||||||#
#||||||#
#||||||#
########""" |> AsciiArt |> toColors);
 
              (LeftL, """
########
#//////#
#/####/#
#/#  #/#
#/#  #/#
#/####/#
#//////#
########""" |> AsciiArt |> toColors);

              (RightS, """
########
#||||||#
#|####|#
#|#  #|#
#|#  #|#
#|####|#
#||||||#
########""" |> AsciiArt |> toColors);

              (LeftS, """
########
#//////#
#//////#
#//##//#
#//##//#
#//////#
#//////#
########""" |> AsciiArt |> toColors)|] 

        module Numbers = 
            let private separatorSize = 2
            let size = 6 + separatorSize

            let private zero = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAsSURBVBhXYzxyfuH/2qIFDMiguS+BgdHR0fE/jAMCMEVMYBILIF0Ch+UJDAAA2REyj0xedgAAAABJRU5ErkJggg=="""
            let private one = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAxSURBVBhXYzxyfuF/BiCoLVoAohia+xLANBOIgAkiA7AETBUyAEtgA/glkO2AsBkYAN2mDF0tHE9JAAAAAElFTkSuQmCC"""
            let private two = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAA2SURBVBhXYzxyfuH/2qIFDMiguS+BgdHR0fE/iAECyAqYYILogAVE4DQKygcLwACKBAIwMAAAldAUZuLmorkAAAAASUVORK5CYII="""
            let private three = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAySURBVBhXY3R0dPzPgAaa+xIYGI+cXwiXqC1aAGUxMLCACGQBEMCrgwlKYwAclicwAABoIhL1lqTgJQAAAABJRU5ErkJggg=="""
            let private four = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAA9SURBVBhXYzxyfuH/2qIFDMiguS+BgQkmCOIgAyYQgS4IAmAJdAAyhdHR0fE/lI8CwJZD2WCVINDcl8AAAGPfFDI3w0rdAAAAAElFTkSuQmCC"""
            let private five = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAA1SURBVBhXY3R0dPzPgAaa+xIYWKBsMAcZ4NTBeOT8QrhEbdECKIuBgQmZgwzAOtAlm/sSGAB7GBRm/DF0+AAAAABJRU5ErkJggg=="""
            let private six = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAwSURBVBhXYzxyfuH/2qIFDMiguS+BgdHR0fE/jIMM4BLIAKsOmLFMYBILwGF5AgMAqUoXlrwDg/8AAAAASUVORK5CYII="""
            let private seven = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAA0SURBVBhXY3R0dPzPgAUwHjm/EEWitmgBmGYCk1AAE2zuS0BIIAuCAFgCJogMUIxCAAYGACa7Dh5oFDUkAAAAAElFTkSuQmCC"""
            let private eight = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAAsSURBVBhXYzxyfuH/2qIFDMiguS+BgdHR0fE/iAECyApI18EEpTEADqMSGACHzhwlr8uZywAAAABJRU5ErkJggg=="""
            let private nine = """data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAA2SURBVBhXYzxyfuH/2qIFDMiguS+BgdHR0fE/iAECyAqYoDQGwGoUCIAloGwUo1hABKblCQwA58IXhDIAisYAAAAASUVORK5CYII="""
        
            let private digits f d = 
                (d:int).ToString() 
                |> Seq.map(fun v ->
                    match v with
                    | '0' -> zero
                    | '1' -> one
                    | '2' -> two
                    | '3' -> three
                    | '4' -> four
                    | '5' -> five
                    | '6' -> six
                    | '7' -> seven
                    | '8' -> eight
                    | '9' -> nine
                    | other -> failwithf "unexpected char %c" other
                    |> f)

            let score f (Score i) = i |> min 999999 |> digits f
            let level f (Level i) = i |> min 99 |> digits f
            let lines f (Lines i) = i |> min 9999 |> digits f

        let displayLines context l = 
            l 
            |> Numbers.lines Html.createImage 
            |> Html.drawImages context Numbers.size 137 81

        let displayLevel context l = 
            l 
            |> Numbers.level Html.createImage 
            |> Html.drawImages context Numbers.size 137 57

        let displayScore context s = 
            s
            |> Numbers.score Html.createImage 
            |> Html.drawImages context Numbers.size 145 25

        let displayGame context g = 
            g.Level |> displayLevel context
            g.Score |> displayScore context
            g.Lines |> displayLines context

        let start context level = 
            let background = Html.createImage background
            background |> Html.drawImage context 0 0
            
            let game = { Level = level; Score = Score 0; Lines = Lines 0 }
            game |> displayGame context
            game

open Gameboy.Tetris

let rec game () =
    Keyboard.reset()
    let canvas = document.getElementsByTagName_canvas().[0]
    let context = canvas.getContext_2d()
    canvas.width <- float Gameboy.width
    canvas.height <- float Gameboy.height
    let boxMap = Gameboy.Tetris.asciiArts |> Map.map(fun _ a -> a |> Html.createPixels context)

    let r = Gameboy.Tetris.Level 0 |> start context

    r

Keyboard.init ()
game ()