            ----------------------------------------------------------------------
--   A Module for Robots  (for use in Programming Task #4, Fall 2021)
--
--   Contains basic type definitions for some silly robots
----------------------------------------------------------------------

module Robots where

type Location = (Integer,Integer)

data Item = Book | Device | Game | Toy
           deriving (Eq,Show)


-- for an advanced robot: first Location is their home, second location
--        is their current location
data Robot = Drone Location (Maybe Item) [String]
           | Rover Location [Item]  [(String,Location)]
           deriving (Show)

data Instr = Move (Integer,Integer)
           | Reset
           | Log String
           | Redact String  -- remove all occurrences of string from log
           | Pickup Item    -- add pickup entry to log
           | DropAll Item   -- add drop entry to log
           deriving (Show)


----------------------------------------------------------------------
-- Two sample robots
----------------------------------------------------------------------

sampleDrone, sampleRover1, sampleRover2 :: Robot

sampleDrone = Drone (5,4) (Just Game) ["Pickup Game","wake"]

sampleRover1 = Rover (2,10) [Book,Device,Toy,Book] [("Pickup Book",(0,5))]
sampleRover2 
    = Rover (-1,-4) [Book] [("Pickup Book",(2,1)),
                            ("DropAll Book",(3,2)),("Pickup Book",(0,5))]


----------------------------------------------------------------------
-- A sample program
----------------------------------------------------------------------

sampleProgram :: [Instr]
sampleProgram = [Move (10,20), Log "moved", Pickup Toy, DropAll Book]