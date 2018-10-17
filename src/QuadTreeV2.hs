module QuadTreeV2 where

type QuadTree2 a = (a,a,a,a)
newtype QuadChunk = QuadChunk (QuadTree2 QuadChunk)

(#) :: QuadChunk -> SubQuadChunk -> QuadChunk
QuadChunk (nw,ne,sw,se) # NW = nw
QuadChunk (nw,ne,sw,se) # NE = ne
QuadChunk (nw,ne,sw,se) # SW = sw
QuadChunk (nw,ne,sw,se) # SE = se

type QuadChunkPop = QuadChunk -> IO Integer
type QuadChunkCons = QuadTree2 QuadChunk -> IO QuadChunk
type QuadChunkCalc = Int -> Integer -> QuadChunk -> IO QuadChunk

dead, alive :: QuadChunk
dead  = QuadChunk (dead,dead,dead,dead)
alive = QuadChunk (alive,alive,alive,alive)

sq_0, sq_1, sq_2 :: [QuadChunk]
sq_0 = [ dead, alive ]
sq_1 = [ QuadChunk (nw,ne,sw,se)
       | nw <- sq_0, ne <- sq_0, sw <- sq_0, se <- sq_0 ]
sq_2 = [ QuadChunk (nw,ne,sw,se)
       | nw <- sq_1, ne <- sq_1, sw <- sq_1, se <- sq_1 ]

surroundings :: QuadChunkPop -> QuadChunk -> QuadChunk -> QuadChunk
                      -> QuadChunk -> QuadChunk -> QuadChunk
                      -> QuadChunk -> QuadChunk -> QuadChunk -> IO QuadChunk
surroundings pop nw nn ne
             ww cc ee
             sw ss se = do
    pnw <- pop nw;  pnn <- pop nn;  pne <- pop ne;
    pww <- pop ww;                  pee <- pop ee;
    psw <- pop sw;  pss <- pop ss;  pse <- pop se;
    let count = pnw + pnn + pne
              + pww       + pee
              + psw + pss + pse
    return $ if count == 2 then  cc   else
             if count == 3 then alive else dead

all_surroundings :: QuadChunkCons -> QuadChunkPop -> QuadTree2 QuadChunk -> IO QuadChunk
all_surroundings cons pop (nw,ne,sw,se) = do
    nwr <- surroundings pop (nw#NW) (nw#NE) (ne#NW)
                        (nw#SW) (nw#SE) (ne#SW)
                        (sw#NW) (sw#NE) (se#NW)
    ner <- surroundings pop (nw#NE) (ne#NW) (ne#NE)
                        (nw#SE) (ne#SW) (ne#SE)
                        (sw#NE) (se#NW) (se#NE)
    swr <- surroundings pop (nw#SW) (nw#SE) (ne#SW)
                        (sw#NW) (sw#NE) (se#NW)
                        (sw#SW) (sw#SE) (se#SW)
    ser <- surroundings pop (nw#SE) (ne#SW) (ne#SE)
                        (sw#NE) (se#NW) (se#NE)
                        (sw#SE) (se#SW) (se#SE)
    cons (nwr,ner,swr,ser)