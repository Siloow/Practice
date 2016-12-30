import Control.Applicative ((<$>),(<*>))

type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

modulo :: Double -> Double -> Double
modulo a b = fromIntegral $ mod (round a) (round b)

operatorRegister :: Register
operatorRegister = [
            ("+", (+)),
            ("-", (-)),
            ("*", (*)),
            ("/", (/)),
            ("%", modulo)
        ]

main = print $ calculate "2 * 5 + 5 % 2"

calculate :: String -> Maybe Double
calculate = eval operatorRegister . words

eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing
eval _ [] = Nothing
eval _ [number] = Just $ read number
eval ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_,[]) -> eval rest unparsed
        (beforeOperator, afterOperator) ->
            function
                <$> (eval operatorRegister beforeOperator)
                <*> (eval operatorRegister $ drop 1 afterOperator)
