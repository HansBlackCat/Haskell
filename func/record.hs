

data Person = Person {firstName :: String
                    ,familyName :: String
                    ,height :: Float
                    ,weight :: Float} deriving(Show)

data Vector a = Vector a a a deriving(Show)

bMI :: Person -> Float
bMI (Person _ _ a b) = b / (a^2)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector a b c) = Vector (i+a) (j+b) (k+c)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k) (Vector a b c) = i*a + j*b + k*c

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

