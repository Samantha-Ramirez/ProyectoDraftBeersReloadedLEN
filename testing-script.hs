import Proyecto1
import Test.HUnit

-- Test cases initialBarrels
test11 :: Test
test11 = TestCase (assertEqual "Test initialBarrels 1" 
                  ((10,10), (7,5), (3,2))
                  (initialBarrels (10, 10) (7, 5) (3, 2)))

test12 :: Test
test12 = TestCase (assertEqual "Test initialBarrels 2" 
                  ((100,0), (200,1), (300,2))
                  (initialBarrels (100, 0) (200, 1) (300, 2)))

test13 :: Test
test13 = TestCase (assertEqual "Test initialBarrels 3" 
                  ((0,0), (0,0), (0,0))
                  (initialBarrels (0, 0) (0, 0) (0, 0)))

test14 :: Test
test14 = TestCase (assertEqual "Test initialBarrels 4" 
                  ((0, 0), (0, 0), (0, 0))
                  (initialBarrels (0, 1) (0, 2) (0, 3)))


-- Test cases iSolution
test21 :: Test
test21 = TestCase (assertEqual "Test iSolution 1" 
                  True
                  (iSolution ((1,1), (2,2), (3,3)) 0))

test22 :: Test
test22 = TestCase (assertEqual "Test iSolution 2" 
                  True
                  (iSolution ((1,1), (2,2), (3,3)) 1))

test23 :: Test
test23 = TestCase (assertEqual "Test iSolution 3" 
                  False
                  (iSolution ((0,0), (0,0), (0,0)) 0))

test24 :: Test
test24 = TestCase (assertEqual "Test iSolution 4" 
                  False
                  (iSolution ((100,100), (200,200), (300,299)) 300))


-- Test cases addBeer
test31 :: Test
test31 = TestCase (assertEqual "Test addBeer 1" 
                  ((9,9), 1)
                  (addBeer 10 (9,0)))

test32 :: Test
test32 = TestCase (assertEqual "Test addBeer 2" 
                  ((999,999), 999)
                  (addBeer 1000 (999,998)))

test33 :: Test
test33 = TestCase (assertEqual "Test addBeer 3" 
                  ((1000,1000), 1000)
                  (addBeer 1000 (1000,1000)))

test34 :: Test
test34 = TestCase (assertEqual "Test addBeer 4" 
                  ((0,0), 1000)
                  (addBeer 1000 (0,0)))

-- Test cases findBestSolution

test43 :: Test
test43 = TestCase (assertEqual "Test findBestSolution 3" 
                  (0, ((1000, 0), (500, 500), (400, 400)))
                  (findBestSolution 200 ((1000, 0), (500, 500), (400, 400))))

test44 :: Test
test44 = TestCase (assertEqual "Test findBestSolution 4" 
                  (50, ((400, 400), (600, 600), (500, 500)))
                  (findBestSolution 600 ((400, 400), (600, 550), (500, 500))))


test46 :: Test
test46 = TestCase (assertEqual "Test findBestSolution 6" 
                  (1,  ((10, 1), (5, 3), (1, 1)))
                  (findBestSolution 3 ((10, 1), (5, 2), (1, 1))))

test47 :: Test
test47 = TestCase (assertEqual "Test findBestSolution 7" 
                  (5,  ((10, 7), (5, 5), (1, 1)))
                  (findBestSolution 7 ((10, 2), (5, 10), (1, 1))))

test41 :: Test
test41 = TestCase (assertEqual "Test findBestSolution 1" 
                  (400, ((1000, 600), (500, 500), (400, 400)))
                  (findBestSolution 600 ((1000, 0), (500, 600), (400, 500))))

test42 :: Test
test42 = TestCase (assertEqual "Test findBestSolution 2" 
                  (0, ((1000, 200), (500, 500), (400, 400)))
                  (findBestSolution 500 ((1000, 0), (500, 600), (400, 500))))

test45 :: Test
test45 = TestCase (assertEqual "Test findBestSolution 5" 
                  (0,  ((0, 1), (0, 0), (0, 1)))
                  (findBestSolution 1 ((0, 1), (0, 0), (0, 1))))



-- Group all tests
tests :: Test
tests = TestList [test11, test12, test13, test14, test21, test22, test23, test24, test31, test32, test33, test34, test41, test42, test43, test44, test45, test46, test47]

-- Run tests
main :: IO Counts
main = runTestTT tests