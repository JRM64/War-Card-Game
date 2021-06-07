module War where

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Eq, Ord, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show)

type Card = (Rank, Suit)
type Deck = [Card]
type Hand = [Card]

--------------------
-- not used
playerOneWinner :: (Hand, Hand) -> Bool
playerOneWinner (_ , [])  = True
playerOneWinner (_ , _)   = False

playerTwoWinner :: (Hand, Hand) -> Bool
playerTwoWinner ([], _)   = True
playerTwoWinner (_ , _)   = False
--------------------

makeDeck :: Deck
makeDeck = [(r,s) | r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace],
                    s <- [Hearts, Clubs, Diamonds, Spades]]

dealCards :: Deck -> (Hand,Hand) -> (Hand,Hand)
dealCards [] (p1,p2) = (p1,p2)
dealCards (x:y:t) (p1,p2) = dealCards t (x:p1, y:p2)
dealCards _ _ = error "unexpected"

-- need shuffling -- why is this a problem?


-- how to loop? how to stop game? 
play :: (Hand,Hand) -> String
play (p1, p2)
    | null p1  = "player 2 wins"
    | null p2  = "player 1 wins"
    | p == 1   = play ( tail p1 ++ [topcardp1, topcardp2] , tail p2 )
    | p == 2   = play ( tail p1, tail p2 ++ [topcardp1, topcardp2] )
    -- | p == 0   = "war!"   -- tie, "War scenario"
    | p == 0 && s == 1 = play ( tail p1 ++ [topcardp1, topcardp2] , tail p2 )
    | p == 0 && s == 2 = play ( tail p1, tail p2 ++ [topcardp1, topcardp2] )
    | otherwise = error "unexpected"
    where
        topcardp1 = head p1
        topcardp2 = head p2
        p = betterCard topcardp1 topcardp2
        s = betterSuit topcardp1 topcardp2


betterCard :: Card -> Card -> Int
betterCard (r1,_) (r2,_)
    | r1 > r2   = 1   -- first card better
    | r2 > r1   = 2   -- second card better
    | otherwise = 0   -- tie


betterSuit :: Card -> Card -> Int
betterSuit c1 c2
    | getSuit c1 > getSuit c2   = 1   -- first card better
    | getSuit c2 > getSuit c1   = 2   -- second card better
    | otherwise                 = error ("unexpected" ++ show c1 ++ " " ++ show c2)

getSuit :: Card -> Suit
getSuit c = snd c


emptyHand = []
run = play (dealCards makeDeck (emptyHand, emptyHand))