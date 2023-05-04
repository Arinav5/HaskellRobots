-- Arina Vardanyan avardany@syr.edu 
-- This work was done all by myself and I have consulted no sources other than slides and other class materials.
import Robots
import Data.Maybe


-- PURPOSE
-- move (x,y) rbt has the effect of moving rbt to the location that is x units east and y units
--north of its current location.

-- DEFINITION 
move :: (Integer,Integer) -> Robot -> Robot
move (x,y) (Drone (a,b) c d) = Drone (a + x, b + y) c d
move (x,y) (Rover a b [(c,(d,e))]) = Rover a b [(c,(d+x, e+y))]

-- TESTS
a1 = move (10,20) sampleDrone -- evaluates to Drone (15,24) (Just Game) ["Pickup Game","wake"]. Given test case
a2 = move (10,20) sampleRover1 -- evaluates to Rover (2,10) [Book,Device,Toy,Book] [("Pickup Book",(10,25))]. Chose this test because it should change the second location for a Rover. 
a3 = move (-5, 0) sampleDrone -- evalutes to Drone (0,4) (Just Game) ["Pickup Game","wake"]. Chose this test because it has negative numbers and I wanted to make sure moving backwards also worked. 

-- PURPOSE
-- reset rbt has the effect of erasing the log of rbt and removing any items held by rbt.

-- DEFINITION
reset :: Robot -> Robot
reset (Drone l Nothing s) = Drone l Nothing []
reset (Drone l x s) = Drone l Nothing []
reset (Rover l i s) = Rover l [] []

-- TESTS
b1 = reset sampleDrone -- evaluates to Drone (5,4) Nothing []. Given test case. 
b2 = reset (Drone (5,4) Nothing ["Pick up game"]) --evalutes to Drone (5,4) Nothing []. Chose this test case because it has Nothing 
b3 = reset sampleRover2 -- evaluates to Rover (-1,-4) [] []. Given test case. 

-- PURPOSE
-- addEntry str rbt returns the robot state obtained by adding str to the front of rbt’s log.

-- DEFINITION
addEntry :: String -> Robot -> Robot
addEntry str (Drone l x s) = (Drone l x (str:s))
addEntry str (Rover l x s) = (Rover l x ((str,l):s))

-- TESTS
c1 = addEntry "sleep" sampleDrone -- evaluates to Drone (5,4) (Just Game) ["sleep","Pickup Game","wake"]. Given test case.
c2 = addEntry "sleep" sampleRover1 --evaluates to Rover (2,10) [Book,Device,Toy,Book] [("sleep",(2,10)),("Pickup Book",(0,5))]. Given tests case. 
c3 = addEntry "sleep" sampleRover2 -- evalutes to Rover (-1,-4) [Book] [("sleep",(-1,-4)),("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))]. Chose this case because it has negative position. 

-- PURPOSE
-- redact str rbt removes all occurrences of the string str from its log.

-- DEFINITION
redact :: String -> Robot -> Robot
redact str (Drone loc itm []) =  (Drone loc itm [])
redact str (Drone loc itm x) = (Drone loc itm (filter (\ y -> y /= str) x))
redact str (Rover loc itm x) = (Rover loc itm (filter (\ (z,y) -> z /= str) x))

-- TESTS
d1 = redact "Pickup Game" sampleDrone --evaluates to Drone (5,4) (Just Game) ["wake"]. Given test case. 
d2 = redact "Pickup Book" sampleRover2 --evaluates to Rover (-1,-4) [Book] [("DropAll Book",(3,2))]. Given test case.
d3 = redact "Pickup Book" sampleRover1 --evalutes to Rover (2,10) [Book,Device,Toy,Book] []. Chose this case because wanted to check that returned empty list. 

-- PURPOSE
-- pickup thing rbt returns the robot state obtained when rbt picks up thing.

-- DEFINITION
pickup :: Item -> Robot -> Robot
pickup itm (Drone loc i x) = (Drone loc (Just Book) (("Pickup " ++ show(itm)):x))
pickup itm (Rover loc i x) = (Rover loc (itm:i) (("Pickup " ++ show(itm), loc):x))

-- TESTS
e1 = pickup Toy sampleDrone --evaluates to Drone (5,4) (Just Toy) ["Pickup Toy","Pickup Game","wake"]. Given test case
e2 = pickup Toy sampleRover1 --evaluates to Rover (2,10) [Toy,Book,Device,Toy,Book] [("Pickup Toy",(2,10)),("Pickup Book",(0,5))]. Given test case. 
e3 = pickup Book sampleRover2 --evaluates to Rover (-1,-4) [Book,Book] [("Pickup Book",(-1,-4)),("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))]. Chose this case because it only has one Book and wanted to check if it would add another. 



-- PURPOSE
-- dropAll thing rbt returns the robot state obtained when rbt drops all copies of thing
-- that it may be holding.

-- DEFINTION
dropAll :: Item -> Robot -> Robot
dropAll itm (Drone loc i x)
    | [itm] == (maybeToList i) = Drone loc Nothing (("DropAll " ++ show(itm)):x)
    | otherwise = Drone loc i (("DropAll " ++ show(itm)):x)
dropAll itm (Rover loc [] x) = (Rover loc [] x)
dropAll itm (Rover loc i x) = Rover loc (filter (\ y -> y /= itm) i) ((("DropAll " ++ show(itm),loc):x))

-- TESTS
f1 = dropAll Game sampleDrone --evaluates to Drone (5,4) Nothing ["DropAll Game","Pickup Game","wake"]. Given test case.
f2 = dropAll Book sampleDrone --evaluates to Drone (5,4) (Just Game) ["DropAll Book","Pickup Game","wake"]. Chose this case because sampleDrone doesn't have a Book. 
f3 = dropAll Book sampleRover1 --evaluates to Rover (2,10) [Device,Toy] [("DropAll Book",(2,10)),("Pickup Book",(0,5))]. Given test case. 
f4 = dropAll Toy sampleDrone -- evaluates to Drone (5,4) (Just Game) ["DropAll Toy","Pickup Game","wake"]
f5 = dropAll Book sampleRover2 -- evaluates to Rover (-1,-4) [] [("DropAll Book",(-1,-4)),("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))]. Chose this case because sampleRover2 only has one item. 


-- PURPOSE
-- perform i rbt returns the resulting state when rbt performs the instruction i.

-- DEFINTION
perform :: Instr -> Robot -> Robot
perform (Move (x,y)) rob = move (x,y) rob
perform Reset rob = reset rob 
perform (Log str) rob = addEntry str rob
perform (Redact str) rob = redact str rob
perform (Pickup itm) rob = pickup itm rob
perform (DropAll itm) rob = dropAll itm rob

-- TESTS
g1 = perform (Move (10,20)) sampleDrone --evaluates to Drone (15,24) (Just Game) ["Pickup Game","wake"]. Given test case
g2 = perform (Redact "Pickup Book") sampleRover2 --evaluates to Rover (-1,-4) [Book] [("DropAll Book",(3,2))]. Given test case. 
g3 = perform (DropAll Book) sampleRover1 --evaluates to Rover (2,10) [Device,Toy] [("DropAll Book",(2,10)),("Pickup Book",(0,5))]. Given test case
g4 = perform Reset sampleDrone --evaluates to Drone (5,4) Nothing []. Chose this case to test the Reset functionality. 
g5 = perform (Log "sleep") sampleRover1 --evaluates to Rover (2,10) [Book,Device,Toy,Book] [("sleep",(2,10)),("Pickup Book",(0,5))]. Chose this case to test the Log functionality. 
g6 = perform (Pickup Device) sampleRover2 --evaluates to Rover (-1,-4) [Device,Book] [("Pickup Device",(-1,-4)),("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))]. Chose this case to test the Pickup functionality. 

