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

tptlinfo_ep = PtlInfo { ptlid = 1
                      , idup = -11
                      , istup = -1
                      , mothup = (0,0)
                      , icolup = (0,0)
                      , pup = (0, 0, 7000, 7000,0)
                      , vtimup = 0
                      , spinup = 1 } 

tptlinfo_em = PtlInfo { ptlid = 2
                      , idup = 11
                      , istup = -1
                      , mothup = (0,0)
                      , icolup = (0,0)
                      , pup = (0, 0, -7000, 7000,0)
                      , vtimup = 0
                      , spinup = -1 } 


energy (x,y,z) = sqrt (x*x + y*y + z*z)

tptlinfo_u (x,y,z) = PtlInfo { ptlid = 3
                             , idup = 2
                             , istup = -1
                             , mothup = (0,0)
                             , icolup = (0,0)
                             , pup = (x, y, z, energy (x,y,z), 0)
                             , vtimup = 0
                             , spinup = -1 } 

tptlinfo_ubar (x,y,z) = PtlInfo { ptlid = 4
                                , idup = -2
                                , istup = -1
                                , mothup = (0,0)
                                , icolup = (0,0)
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

eventXML :: String -> String
eventXML str = "<event>\n" ++ str ++ "\n</event>"

genEvent :: ((Double,Double, Double) -> LHEvent) 
            ->  MTGen -> Double -> Double -> IO () -- B.ByteString
genEvent evgen gen pt eta = do
    -- phi <- (2.0 * pi *) <$> random gen :: IO Double    
    plst <- sequence (replicate 100 (go gen pt eta))
    let evts = map evgen plst
    mapM_ (putStrLn . eventXML . formatLHEvent) evts


testlhe :: (Double,Double,Double) -> LHEvent
testlhe (x,y,z) = LHEvent tevinfo [tptlinfo_ep, tptlinfo_em, tptlinfo_u (x,y,z), tptlinfo_ubar (-x,-y,-z)]

main :: IO ()
main = do
  putStrLn "standard candle lhe"
  gen <- newMTGen Nothing
  genEvent testlhe gen 100 1.0
  -- putStrLn $ formatLHEvent (testlhe (200,300,600))


