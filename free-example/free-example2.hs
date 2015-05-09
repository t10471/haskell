
import Control.Monad.Trans.Free

--食材
type Ingredient = String
--火力
data Power = Low | Medium | Strong deriving Show
--調理器具
type Cookware = String

data Cook next =
      Heat Int Power Cookware next
    | Mix [Ingredient] next
    | Bake Int [Ingredient] next
    | Done

instance Functor Cook where
    fmap f (Heat n p ware next) = Heat n p ware (f next)
    fmap f (Mix ings next)      = Mix ings (f next)
    fmap f (Bake n ings next)   = Bake n ings (f next)
    fmap f Done                 = Done

type RecipeT = FreeT Cook

cooking :: RecipeT IO () -> IO ()
cooking m = runFreeT m >>= \r -> case r of
  (Free (Heat n p ware next)) -> putStrLn ("Heat " ++ ware ++ show n ++ " minutes on " ++ show p ++ " heat") >> cooking next
  (Free (Mix ings next))      -> putStrLn ("Mix " ++ show ings) >> cooking next
  (Free (Bake n ings next))   -> putStrLn ("Bake " ++ show ings ++ " " ++ show n ++ "minutes") >> cooking next
  (Free Done)                 -> putStrLn "finish !!!"

bake :: Monad m => Int -> [Ingredient] -> RecipeT m ()
bake n ings = liftF $ Bake n ings ()

mix :: Monad m => [Ingredient] -> RecipeT m ()
mix ings = liftF $ Mix ings ()

heat :: Monad m => Int ->Power -> Cookware -> RecipeT m ()
heat n pw ware = liftF $ Heat n pw ware ()

done :: Monad m => RecipeT m ()
done = liftF Done

omelet :: Monad m => RecipeT m ()
omelet = do
    let egg = ["egg"]
    mix egg
    heat 2 Strong "pan"
    bake 3 egg
    done

main :: IO ()
main = do
  cooking omelet
  putStrLn "end"


