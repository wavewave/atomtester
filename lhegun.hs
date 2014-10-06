import Control.Applicative
import System.Random.Mersenne
-- 
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Formatter


tevinfo = EvInfo { nup = 4
                 , idprup = 0
                 , xwgtup = 0.8830800e-7 
                 , scalup = 0.1183216e+3  
                 , aqedup = 0.7546771e-2  
                 , aqcdup = 0.1135437
                 } 

tptlinfo_ep (x,y,z) = PtlInfo { ptlid = 1
                      , idup = -11
                      , istup = -1
                      , mothup = (0,0)
                      , icolup = (0,0)
                      , pup = (0, 0, energy (x,y,z), energy (x,y,z), 0)
                      , vtimup = 0
                      , spinup = 1 } 

tptlinfo_em (x,y,z) = PtlInfo { ptlid = 2
                      , idup = 11
                      , istup = -1
                      , mothup = (0,0)
                      , icolup = (0,0)
                      , pup = (0, 0, - (energy (x,y,z)), energy (x,y,z), 0 )
                      , vtimup = 0
                      , spinup = -1 } 


energy (x,y,z) = sqrt (x*x + y*y + z*z)

tptlinfo_u (x,y,z) = PtlInfo { ptlid = 3
                             , idup = 2
                             , istup = 1
                             , mothup = (1,2)
                             , icolup = (501,0)
                             , pup = (x, y, z, energy (x,y,z), 0)
                             , vtimup = 0
                             , spinup = -1 } 

tptlinfo_ubar (x,y,z) = PtlInfo { ptlid = 4
                                , idup = -2
                                , istup = 1
                                , mothup = (1,2)
                                , icolup = (0,501)
                                , pup = (x, y, z, energy (x,y,z), 0)
                                , vtimup = 0
                                , spinup = 1 } 

go :: MTGen -> Double -> Double -> IO (Double,Double,Double)
go gen pt eta = do
    phi <- (2.0 * pi *) <$> random gen :: IO Double
    
    -- costh <- (\x -> x * 2.0 - 1.0 ) <$> random gen :: IO Double 
    let costh = etatocosth eta
    let sinth = sqrt (1 - costh*costh)
        pz = pt * costh / sinth
        px = pt * cos phi
        py = pt * sin phi
    return (px,py,pz)  

lheXML :: String -> String
lheXML str = "<LesHouchesEvents version=\"1.0\">\n<init>\n      -11       11  0.70000000000E+04  0.70000000000E+04 0 0       0       0 3   1\n    5.220106E+00  5.384128E-01  1.000000E+00    81\n  2.602564E-01  1.062492E-01  1.000000E+00    82\n</init>\n"
              ++ str ++ "</LesHouchesEvents>\n"

eventXML :: String -> String
eventXML str = "<event>\n" ++ str ++ "\n</event>\n"

genEvent :: ((Double,Double, Double) -> LHEvent) 
            ->  MTGen -> Double -> Double -> IO String -- () -- B.ByteString
genEvent evgen gen pt eta = do
    -- phi <- (2.0 * pi *) <$> random gen :: IO Double    
    plst <- sequence (replicate 10000 (go gen pt eta))
    let evts = map evgen plst
    (return . concatMap eventXML . map formatLHEvent) evts


testlhe :: (Double,Double,Double) -> LHEvent
testlhe (x,y,z) = LHEvent tevinfo [ tptlinfo_ep (x,y,z)
                                  , tptlinfo_em (x,y,z)
                                  , tptlinfo_u (x,y,z)
                                  , tptlinfo_ubar (-x,-y,-z)]

main :: IO ()
main = do
  gen <- newMTGen Nothing
  str <- genEvent testlhe gen 100 1.0
  putStrLn (lheXML str)
  -- putStrLn $ formatLHEvent (testlhe (200,300,600))


