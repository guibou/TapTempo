{-# OPTIONS -Wall #-}
import Bf (precision)
import Data.Char (ord)

-- programme brainfuck
tapTempo :: String
tapTempo = displayString "Appuyer sur une touche en cadence (q pour quitter).\n" ++ tempoLoop ++ displayString "Au revoir !\n"
  where
    loopQ middle = "[-]," ++ replicate 113 '-' ++ "[" ++ middle ++ "," ++ replicate 113 '-' ++ "]"
    displayChar c = "[-]" ++ replicate (ord c) '+' ++ "." ++ "[-]"
    displayString s = concatMap displayChar s
    setConst x = "[-]" ++ replicate x '+'
    displayInt = "[>>+>+<<<-]>>>[<<<+>>>-]<<+>[<->[>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-] ++++++++[<++++++>-]>[<<+>>-]>[<<+>>-]<<]>]<[->>++++++++[<++++++>-]]<[.[-]<]<"

    -- x = current offset - 1
    -- y = current offset
    -- temp0 = +1
    -- temp1 = +2
    -- temp2 = +3
    -- temp3 = +4

    -- end at x
    divAl = ">[-] >[-] >[-] >[-] <<<<<[>>+<<-] >>[<[>>+>+<<<-] >>>[<<<+>>>-] <[>+ <<-[>>[-]>+<<<-] >>>[<<<+>>>-] <[<- [<<<->>>[-]]+ >-] <-] <<<+ >>]" ++ "[-]>[-]>[-]>[-]<<<<[-]<"

    tempoLoop = loopQ (">" ++ displayString "\bTempo : " ++ "<" ++ "<+>" ++ "" ++ (">" ++ setConst (precision * 60) ++ ">[-]<<[>>+<<-]" ++ ">>" ++ divAl ++ displayInt ++ "[-]<") ++ ">" ++ displayString " bpm.\n" ++ "<")

main :: IO ()
main = putStrLn tapTempo
