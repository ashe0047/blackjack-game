-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import           Parser.Instances
-- Rules of the game
import Data.Maybe
import Control.Monad
import Data.List
import GHC.Float.RealFracMethods (int2Double, int2Float)



-- You can add more imports if you need them
-- memory data parser
data MemData = MemData {getUpcard:: Hand, getAction:: [Action], getBid:: Points, getNumCards:: Int, getRunningCount:: Int}


--serialisation functions
printAct:: [Action] -> String
printAct lst= "["++next lst++"]"
                    where
                        next [] = ""
                        next (y:ys) = act++sep++next ys
                                    where
                                        act = case y of
                                            (Bid _) -> "bid"
                                            Hit -> "ht"
                                            Stand -> "st"
                                            (DoubleDown _) -> "dd"
                                            (Split _) -> "sp"
                                            (Insurance _) -> "ins"
                                        sep = if null ys then "" else ","

instance Show MemData where
    show (MemData upcard act bid numc rcount) = show upcard ++ "_" ++ printAct act ++ "_" ++ show bid ++ "_" ++ show numc ++ "_" ++ show rcount


-- Parsers
isIn :: [Char] -> Parser Char
isIn c = do
    v <- character
    let next = if v `elem` c then pure else const $ unexpectedCharParser v
    next v

--int parser
int:: Parser Int
int = do
    i <- list digit
    pure $ read i

-- list parser 
list1 :: Parser a -> Parser [a]
list1 p = do
    p' <- p
    p'' <- list p
    pure (p':p'')

list :: Parser a -> Parser [a]
list p = (list1 p >>= pure) ||| (p >>= \p' -> pure [p'])

--string parser
str:: String -> Parser String
str = traverse is

separator:: Parser Char
separator = is '_'

--hand Parser
suit:: Parser Suit
suit = (is 'S' >> pure Spade) ||| (is 'C' >> pure Club) ||| (is 'D' >> pure Diamond) ||| (is 'H' >> pure Heart)

rank:: Parser Rank
rank = (is 'A' >> pure Ace) ||| (is '2' >> pure Two) ||| (is '3' >> pure Three) ||| (is '4' >> pure Four) ||| (is '5' >> pure Five) ||| (is '6' >> pure Six) ||| (is '7' >> pure Seven) ||| (is '8' >> pure Eight) ||| (is '9' >> pure Nine) ||| (is 'T' >> pure Ten) ||| (is 'J' >> pure Jack) ||| (is 'Q' >> pure Queen) ||| (is 'K' >> pure King)

card:: Parser Card
card = do
    a <- suit
    b <- rank
    pure (Card a b)

listCard:: Parser Card
listCard = (do
    a <- card
    is ','
    pure a) ||| (card >>= pure)

hand:: Parser Hand
hand = (do
    is '['
    a <- list listCard
    is ']'
    pure a) ||| (is '[' >> is ']' >> pure [])



--listAction Parser
action:: Parser Action
action = (str "st" >> pure Stand ) ||| (str "ht" >> pure Hit ) ||| (str "dd" >> pure (DoubleDown 0) ) ||| (str "sp" >> pure (Split 0) ) ||| (str "ins" >> pure (Insurance 0) ) ||| (str "bid" >> pure (Bid 0) )

actions:: Parser Action
actions = (do
    a <- action
    is ','
    pure a
    ) ||| (action >>= pure)

listAction :: Parser [Action]
listAction =( do
    is '['
    a <- list actions
    is ']'
    pure a) ||| (is '[' >> is ']' >> pure [])

--lastBid Parser
lastBid:: Parser Points
lastBid = int >>= \p -> pure p

--numCards Parser
numCards:: Parser Int
numCards = int >>= \p -> pure p

--runningCount Parser
runningCount:: Parser Int
runningCount = int >>= \p -> pure p

digit:: Parser Char
digit = isIn "1234567890-"

--memory string Parser
mem:: Parser MemData
mem = do
    a <- hand
    separator
    d <- listAction
    separator
    e <- lastBid
    separator
    f <- numCards
    separator
    g <- runningCount
    pure (MemData a d e f g)


--retrieving memory from parse result
getMem:: ParseResult a -> a
getMem (Result _ memdata) = memdata
getMem (Error err) = error (show err)


-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard c d e f g h
    | isNothing c = bidFunc g d e f
    | otherwise = basicStrat (fromJust c) h g d e f


--bidding function
bidFunc :: Maybe String -> [PlayerPoints] -> [PlayerInfo] -> PlayerId -> (Action, String)
bidFunc memstr curpts pinfo pid
    | isNothing memstr = (Bid minBid, "[]_[]_"++show minBid++"_"++show numcards++"_0")
    | otherwise = (bid, newmem)
                    where
                        bid = bidAmt (getRunningCount memdata) (getNumCards memdata)
                        --parse mem string
                        memdata = getMem $ parse mem (fromJust memstr)
                        --rebuild mem string
                        newmem = show $ uncurry (MemData (getUpcard memdata) [] (bidval bid)) updatecurrun
                        numcards = numDecks*52
                        bidval (Bid y) = y
                        plyorder = plyOrdOp pid curpts
                        updatecurrun = updateRun ((head . getUpcard) memdata) (reverse$snd plyorder) pinfo (getNumCards memdata) (getRunningCount memdata)


--takes in running count and cards left and compute the bid amount
bidAmt:: Int -> Int -> Action
bidAmt rcount numcard
    | betunit <= 2 = Bid minBid
    | betunit <= 19 = Bid (betunit*betperunit)
    | otherwise = Bid maxBid
    where
        betunit = floor((/) (int2Double rcount) deckleft) - 1
        betperunit = div maxBid 19
        deckleft = if round((/) (int2Double numcard) 52) <= (0 :: Integer) then 1 else (/) (int2Double numcard) 52



--Action generator function 
--basic strategy
basicStrat :: Card -> Hand -> Maybe String -> [PlayerPoints] -> [PlayerInfo] -> PlayerId -> (Action, String)
basicStrat c h memstr ppts pinfo pid
    | isSoft h = (soft, newmem soft)
    | otherwise = (hard, newmem hard)
    where
        memdata = getMem $ parse mem (fromJust memstr)
        newmem move = show $ uncurry (MemData [c] (moveupdate move) (bidupdate move)) updatecurrun
        soft = softMove c h (getBid memdata) (getAction memdata)
        hard = hardMove c h (getBid memdata) (getAction memdata)
        --update bid
        bidupdate move = case move of
            DoubleDown xbid -> xbid * 2
            _ -> getBid memdata
        moveupdate move = case move of
            DoubleDown pts -> [DoubleDown pts]
            _ | not (null$getAction memdata) -> move:getAction memdata
              | otherwise -> []
        --update card left and running count
        plyorder = plyOrdOp pid ppts
        updatecurrun
            | (null . playerInfoHand . head)(filter (\a->getId a == pid) pinfo) = updateRun c (reverse$fst plyorder) pinfo (getNumCards memdata) (getRunningCount memdata)
            | otherwise = (getNumCards memdata, getRunningCount memdata)


--updated running count and card left in deck
updateRun:: Card -> [PlayerId] -> [PlayerInfo] -> Int -> Int -> (Int, Int)
updateRun upcard plyord pinfo cardsleft rcount = (updatedcards, updatedcount)
        where
            updatedcards
                | cardsleft - cardchg > 0 = cardsleft - cardchg
                | otherwise = (numDecks*52) + cardsleft - cardchg
            updatedcount
                | cardsleft - cardchg > 0 = rcount + sum (cardToPts<$>hands) + dealerpts
                | otherwise = sum (cardToPts<$>take (abs cardsleft-cardchg) hands) +dealerpts
            cardchg = length hands
            hands = concat(playerInfoHand<$>filter (\a->getId a `elem` plyord) pinfo)
            dealerpts = cardToPts upcard

--hi-lo converter function
cardToPts:: Card -> Int
cardToPts card
    | contain [Two .. Six] = 1
    | contain [Seven .. Nine] = 0
    | contain ([Ten .. King]++[Ace]) = -1
    | otherwise = 0
    where
        contain = elem (getRank card)

--partition play order into before and after current player
plyOrdOp:: PlayerId -> [PlayerPoints] -> ([PlayerId],[PlayerId])
plyOrdOp pid ppts = splitAt ind idlst
    where
        ind = fromJust (elemIndex pid idlst)
        idlst = (getId<$>ppts)++["dealer"]


--check if hand is soft
isSoft:: Hand -> Bool
isSoft h = elem Ace (getRank<$>h) && (handCalc (filter (\x->getRank x /= Ace)  h) <= 10)

-- check if split is needed
issplit:: Card -> Hand -> Bool
issplit c h = case length h == 2 && getRank (head h) == getRank (head (tail h)) of
        True | rh == Ace || rh == Eight -> True
             | rh == Ten || rh == Five || rh == Four -> False
             | rh == Three || rh == Two -> rc >= 2 && rc <= 7
             | rh == Nine -> not (rc == 7 || rc == 10 || rc == 11)
             | rh == Seven -> rc >= 2 && rc <= 7
             | rh == Six -> rc >= 2 && rc <= 6
             | otherwise -> False
        False -> False
    where
        rh = getRank (head h)
        rc = handCalc [c]


--return action for hardhand
hardMove:: Card -> Hand -> Int -> [Action] -> Action
hardMove c h bid ddmove
    | not (null ddmove) = case head ddmove of
        DoubleDown _ -> Hit
        _ -> Stand
    | issplit c h = Split bid
    | rh >= 17 = Stand
    | rh <= 16 && rh >= 13 = if rc >= 2 && rc <= 6
                                              then Stand
                                              else Hit
    | rh == 12 = if rc >= 2 && rc <= 3
                            then Hit
                         else if rc >= 4 && rc <= 6
                             then Stand
                         else Hit
    | rh == 11 && len <= 2 = DoubleDown bid
    | rh == 10 = if rc == 10 || rc >= 11
                            then Hit
                         else if len <= 2
                             then DoubleDown bid
                         else Hit
    | rh == 9  = if rc >=3 && rc <= 6 && len <= 2
                            then DoubleDown bid
                         else Hit
    | otherwise  = Hit
    where
        rh = handCalc h
        rc = handCalc [c]
        len = length h

--return action for softhand
softMove:: Card -> Hand -> Int -> [Action] -> Action
softMove c h bid ddmove
    | not (null ddmove) = case head ddmove of
        DoubleDown _ -> Hit
        _ -> Stand
    | issplit c h = Split bid
    | rh >= 20 = Stand
    | rh == 19 = if rc == 6 && len <= 2
                            then DoubleDown bid
                         else Stand
    | rh == 18 = if rc >= 2 && rc <= 6 && len <= 2
                            then DoubleDown bid
                         else if rc >= 7 && rc <= 8
                             then Stand
                         else Hit
    | rh == 17 = if rc >= 3 && rc <= 6 && len <= 2
                            then DoubleDown bid
                         else Hit
    | rh == 16 || rh == 15 = if rc >= 4 && rc <= 6 && len <= 2
                                                then DoubleDown bid
                                             else Hit
    | rh == 14 || rh == 13 = if rc >= 5 && rc <= 6 && len <= 2
                                                then DoubleDown bid
                                             else Hit
    | otherwise = Hit
    where
        rh = handCalc h
        rc = handCalc [c]
        len = length h


