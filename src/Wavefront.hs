{-# LANGUAGE OverloadedStrings #-}
module Wavefront (
    loadWavefront
) where

import GHC.Float (double2Float)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr(castPtr)
import Linear.GL
import Control.Applicative
import Control.Monad

import qualified Math.Mesh as M (TriInd(..))
import qualified Graphics as G
import Render
import Attributes
import Math.Mesh (Mesh(..))

data WavefrontVert = WavefrontVert Vec3 Vec3 Vec2

instance Storable WavefrontVert where
    sizeOf _ = 8 * (sizeOf (undefined :: G.CFloat))
    alignment _ = alignment (undefined :: G.CFloat)
    peek ptr = do [vx, vy, vz, nx, ny, nz, u, v] <- mapM (peekElemOff (castPtr ptr)) [0..7]
                  return $ WavefrontVert (V3 vx vy vz) (V3 nx ny nz) (V2 u v)
    poke ptr (WavefrontVert (V3 vx vy vz) (V3 nx ny nz) (V2 u v)) =
            zipWithM_ (pokeElemOff (castPtr ptr)) [0..7] [vx, vy, vz, nx, ny, nz, u, v] 

instance VertexAttribs WavefrontVert where
    schema _ = [ AttribProperties "position" 3 G.Float 0
               , AttribProperties "normal"   3 G.Float (3*floatSize)
               , AttribProperties "texCoord" 2 G.Float (6*floatSize)]
        where floatSize = sizeOf (undefined :: G.CFloat)

loadWavefront :: G.ShaderProgram -> FilePath -> IO (Renderable, Mesh)
loadWavefront shdr file = do
    recs <- fromEither file . parseOnly parseObj <$> B.readFile file
    let vs = [r | V r <- recs]
        vns = [r | VN r <- recs] ++ repeat 0
        vts = [r | VT r <- recs] ++ repeat 0
        verts = zipWith3 WavefrontVert vs vns vts
        faces = V.fromList [f | F f <- recs]
    obj <- makeObject (V.fromList verts) faces shdr
    let mesh = Mesh (V.fromList vs) faces
    return (obj, mesh)

-- obj file lines
data WfLine = V Vec3 | VN Vec3 | VT Vec2 | F M.TriInd
            -- | MtlLib String | UseMtl String
            | Junk

parseObj :: Parser [WfLine]
parseObj =  many ((V <$> parseVert) <|> (F <$> parseFace) <|> (VN <$> parseNorm) <|> (VT <$> parseTex)
                  <|> parseMtl <|> parseUseMtl <|> parseComment)
            <* do rest <- takeByteString 
                  if B.null rest then return ()
                                 else fail (show (B.take 20 rest))

     where
        parseFace = "f " .*> (M.TriInd <$> thenDec <*> thenDec <*> thenDec)
            where thenDec = (subtract 1 <$> decimal) <* skipWhile (not . isSpace) <* skipSpace

        thenFloat = CFloat . double2Float <$> (double <* skipSpace)

        parseVert = "v " .*> (liftA3 V3) thenFloat thenFloat thenFloat
        parseNorm = "vn " .*> (liftA3 V3) thenFloat thenFloat thenFloat
        parseTex = "vt " .*> (liftA2 V2) thenFloat thenFloat

        --parseMtl = MtlLib . B.unpack <$> (string "mtllib " *> takeTill isSpace <* skipSpace)
        --parseUseMtl = UseMtl . B.unpack <$> (string "usemtl " *> takeTill isSpace <* skipSpace)
        parseMtl = string "mtllib " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk
        parseUseMtl = string "usemtl " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk

        parseComment = string "#" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk


fromEither :: String -> Either String b -> b
fromEither message (Left err) = error $ message ++ ": " ++ err
fromEither _ (Right res) = res