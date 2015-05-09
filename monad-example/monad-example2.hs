{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.ST
import Data.Functor.Identity
import Data.Char
import Data.Word
import System.Random.MWC
import Debug.Trace


exIdentity :: IO ()
exIdentity = do
    print $ runIdentity $ Identity 1
    print $ runIdentity $ ex 1
  where
    ex :: Int -> Identity Int
    ex i = (return i) >>= \x -> return (x + 2)


-- Maybe
type EmailAddr = String
data MailPref = HTML | Plain deriving Show

data MailSystem = MS { fullNameDB :: [(String   ,EmailAddr)]
                     , nickNameDB :: [(String   ,EmailAddr)]
                     , prefsDB    :: [(EmailAddr,MailPref)] 
                     }

data UserInfo = User { name  :: String
                     , nick  :: String
                     , email :: EmailAddr
                     , prefs :: MailPref 
                     }

-- UserInfo を変換 
makeMailSystem :: [UserInfo] -> MailSystem
makeMailSystem users = 
    let fullLst = map (\u -> (name u, email u))  users
        nickLst = map (\u -> (nick u, email u))  users
        prefLst = map (\u -> (email u, prefs u)) users
  in MS fullLst nickLst prefLst

-- 名前から情報を取得
getMailPrefs :: MailSystem -> String -> Maybe MailPref
getMailPrefs sys name = do 
    let nameDB = fullNameDB sys
        nickDB = nickNameDB sys
        prefDB = prefsDB sys
    addr <- (lookup name nameDB) `mplus` (lookup name nickDB)
    lookup addr prefDB

{-
 exMaybe "billy"
 exMaybe "bill"
-}
-- print the email preference of the person named on the command-line
exMaybe :: String -> IO ()
exMaybe n = do 
  let users = [ User "Bill Gates"      "billy"       "billg@microsoft.com" HTML,
                User "Bill Clinton"    "slick willy" "bill@hope.ar.us"     Plain,
                User "Michael Jackson" "jacko"       "mj@wonderland.org"   HTML ]
      mailsys = makeMailSystem users
  print (getMailPrefs mailsys n)
-- Maybe 終わり

-- Expect
-- エラータイプ
data LengthError = EmptyString
          | StringTooLong Int -- 5文字以上はエラー
          | OtherError String

instance Show LengthError where
  show EmptyString = "The string was empty!"
  show (StringTooLong len) = 
    "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
  show (OtherError msg) = msg

type LengthMonad = Either LengthError

exExpect :: String -> IO ()
exExpect s = do
  reportResult (calculateLength s)

calculateLength :: String -> LengthMonad Int
calculateLength s = (calculateLengthOrFail s) `catchError` Left

calculateLengthOrFail :: String -> LengthMonad Int
calculateLengthOrFail []            = throwError EmptyString
calculateLengthOrFail s | len > 5   = throwError (StringTooLong len)
                        | otherwise = return len
  where len = length s

reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e)    = putStrLn ("Length calculation failed with error: " ++ (show e))
-- Expect 終わり

-- List
-- 非決定性を持つ計算を行う
-- mplus は結合なので組み合わせの計算ができる
data Parsed = Digit Integer | Hex Integer | Word String deriving Show

parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c = if isHexDigit c 
                          then
                            return (Hex ((n*16) + (toInteger (digitToInt c))))
                          else
                            mzero
parseHexDigit _       _ = mzero

parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c = if isDigit c 
                         then
                           return (Digit ((n*10) + (toInteger (digitToInt c))))
                         else
                           mzero
parseDigit _         _ = mzero
           
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c = if isAlpha c then
                         return (Word (s ++ [c]))
                       else
                         mzero
parseWord _        _ = mzero

parse :: Parsed -> Char -> [Parsed]
parse p c = (parseHexDigit p c) `mplus` (parseDigit p c) `mplus` (parseWord p c)

parseArg :: String -> [Parsed]
parseArg s = do 
    -- init の中身 [Hex 0,Digit 0,Word ""]
    init <- (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
    foldM parse init s

showResult :: String -> IO ()
showResult s = do putStr s
                  putStr ": "
                  print (parseArg s)

{-
exList ["a", "abc", "1", "300"]
-}
exList :: [String] -> IO ()
exList s = do 
    mapM_ showResult s

initList :: [Parsed]
initList = (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
-- List 終わり

-- IO
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

translateString :: String -> String -> String -> String
translateString set1 set2 str = map (translate set1 set2) str

usage :: IOError -> IO ()
usage e = do 
    putStrLn "Usage: ex14 set1 set2"
    putStrLn "Translates characters in set1 on stdin to the corresponding"
    putStrLn "characters from set2 and writes the translation to stdout."
{-
exIO "abc" "bcd" "abcdefgabc"
-}
exIO :: String -> String -> String -> IO ()
exIO s1 s2 c = (putStr $ translateString s1 s2 c)
               `catchError` usage
-- IO 終わり

-- State
data MyType = MT Int Bool Char Int deriving Show

rdmR :: Variate a => (a, a) -> Seed -> (a, Seed)
rdmR range seed = runST $ do
    gen   <- restore seed
    ret   <- uniformR range gen
    seed' <- save gen
    return (ret, seed')

rdmB :: Seed -> (Bool, Seed)
rdmB s = let (r, s1) = rdmR (0,1) s :: (Int, Seed)
    in (conv r, s1)
  where 
    conv i = if i == 0 then False else True

rdmC :: Seed -> (Char, Seed)
rdmC s = let (r, s1) = rdmR (start,end) s :: (Word8, Seed)
    in (w82c r, s1)
  where
    c2w8 :: Char -> Word8
    c2w8 = fromIntegral . fromEnum

    w82c :: Word8 -> Char
    w82c c = toEnum (fromEnum 'a' + (fromIntegral c) - fromEnum 'a')

    start :: Word8
    start = c2w8 'a'

    end :: Word8
    end = c2w8 'z'

makeRandomValue :: Seed -> (MyType, Seed)
makeRandomValue s = let (n,s1) = rdmR (1,100) s :: (Int, Seed)
                        (b,s2) = rdmB s1
                        (c,s3) = rdmC s2
                        (m,s4) = rdmR (-n,n) s3 :: (Int, Seed)

            in (MT n b c m, s4)

getOne :: Variate a => (a,a) -> State Seed a
getOne bounds = do 
    g      <- get
    (x,g') <- return $ rdmR bounds g
    put g'
    return x

getOneB :: State Seed Bool 
getOneB = do 
    g      <- get
    (x,g') <- return $ rdmB g
    put g'
    return x

getOneC :: State Seed Char
getOneC = do 
    g      <- get
    (x,g') <- return $ rdmC g
    put g'
    return x

-- いちいち Seed をもち回らず
-- State から取得している
makeRandomValueST :: Seed -> (MyType, Seed)
makeRandomValueST = runState $ do 
        n <- getOne (1,100) :: State Seed Int
        b <- getOneB
        c <- getOneC
        m <- getOne (-n,n)  :: State Seed Int
        return (MT n b c m)

exState :: IO ()
exState = do
    s <- createSystemRandom >>= save
    print $ fst $ makeRandomValue s
    print $ fst $ makeRandomValueST s
-- State 終わり
   

exRondom :: IO ()
exRondom = withSystemRandom . asGenIO $ \gen -> do
    uniform gen >>= \i -> print (i :: Int )


main :: IO ()
main = do
  putStrLn "end"

