{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module JavaMessage (gen) where

import           System.IO
import           System.Exit
import           System.Directory
import           System.Environment
import           System.FilePath
import           Control.Monad        (when)
import           Data.List         as DL
import           Data.Maybe           (fromMaybe)
import           Text.Printf
import           Text.RE.TDFA
import qualified Data.Map          as M
import           Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text         as T
import           Data.Text.Lazy    as TL
import           Control.Monad
import qualified CmdLine           as CL
import qualified Util              as Util
import qualified Metadata          as Meta


gen :: CL.Options
    -> Meta.Model
    -> IO ()
gen opts model = do
    let namespace = Meta.namespace model
    let p = T.replace "." "/" $ namespace
    let fp = (T.unpack $ CL.outputDir opts) </> (T.unpack $ p)
    createDirectoryIfMissing True fp
    let ns = T.splitOn "." namespace
    let javaName = DL.concat [fp, "/Message.java"]
    withFile javaName WriteMode $ \ h -> do
        hPrintf h "\n"


genField :: T.Text
         -> Meta.Field
         -> T.Text
genField prefix field = case field of
    Meta.Int8Field n s v c -> [st|int8_t #{prefix}#{n}; // #{c}|]
    Meta.UInt8Field n s v c -> [st|uint8_t #{prefix}#{n}; // #{c}|]
    Meta.Int16Field n s v c -> [st|int16_t #{prefix}#{n}; // #{c}|]
    Meta.UInt16Field n s v c -> [st|uint16_t #{prefix}#{n}; // #{c}|]
    Meta.Int32Field n s v c -> [st|int32_t #{prefix}#{n}; // #{c}|]
    Meta.UInt32Field n s v c -> [st|uint32_t #{prefix}#{n}; // #{c}|]
    Meta.Int64Field n s v c -> [st|int64_t #{prefix}#{n}; // #{c}|]
    Meta.UInt64Field n s v c -> [st|uint64_t #{prefix}#{n}; // #{c}|]
    Meta.Float32Field n v c -> [st|float #{prefix}#{n}; // #{c}|]
    Meta.Float64Field n v c -> [st|double #{prefix}#{n}; // #{c}|]
    Meta.ByteStringField n s v c -> [st|std::vector<char> #{prefix}#{n}; // #{c}|]
    Meta.StringField n s v c -> [st|std::string #{prefix}#{n}; // #{c}|]
    Meta.NTStringField n s v c -> [st|char[#{fromMaybe 0 s}] #{prefix}#{n}; // #{c}|]
    Meta.EnumerateField n t s v c -> [st|#{t} #{prefix}#{n}; // #{c}|]
    Meta.EntityField n t v c -> [st|#{t} #{prefix}#{n}; // #{c}|]

genParam :: Meta.Field
         -> T.Text
genParam field = case field of
    Meta.Int8Field n s v c -> [st|int8_t #{n} // #{c}|]
    Meta.UInt8Field n s v c -> [st|uint8_t #{n} // #{c}|]
    Meta.Int16Field n s v c -> [st|int16_t #{n} // #{c}|]
    Meta.UInt16Field n s v c -> [st|uint16_t #{n} // #{c}|]
    Meta.Int32Field n s v c -> [st|int32_t #{n} // #{c}|]
    Meta.UInt32Field n s v c -> [st|uint32_t #{n} // #{c}|]
    Meta.Int64Field n s v c -> [st|int64_t #{n} // #{c}|]
    Meta.UInt64Field n s v c -> [st|uint64_t #{n} // #{c}|]
    Meta.Float32Field n v c -> [st|float #{n} // #{c}|]
    Meta.Float64Field n v c -> [st|double #{n} // #{c}|]
    Meta.ByteStringField n s v c -> [st|std::vector<char> #{n} // #{c}|]
    Meta.StringField n s v c -> [st|std::string #{n} // #{c}|]
    Meta.NTStringField n s v c -> [st|char* #{n}, #{n}Len // #{c}|]
    Meta.EnumerateField n t s v c -> [st|#{t} #{n} // #{c}|]
    Meta.EntityField n t v c -> [st|#{t} #{n} // #{c}|]


-- Generate C++ .cpp Source file content.
genSource :: Meta.Model
          -> Meta.Entity
          -> T.Text
genSource model entity = case entity of
    Meta.Message id ver name fields -> [st|// #{name}
#{name}::#{name}()
  : Message(#{id}, #{ver})
{
}

#{name}::#{name}
  ( #{Util.combinePrefix 2 ", " $ DL.map genParam fields}
  )
  : Message(#{id}, #{ver})
  , #{Util.combinePrefix 2 ", " $ DL.map (genInitializer "m_") fields}
{
}

#{name}::~#{name}()
{
}

std::ostream& operator<<(std::ostream& os, const #{name}& v) {
  os
    << std::dec
    << std::setw(0)
    << "#{name} {"
    << dynamic_cast<const Message&>(v)#{DL.foldl (Util.combineLine 4) "" $ DL.map (genToString "m_") fields}
    << " }";
  return os;
}

|]

    Meta.State id ver name fields -> [st|// #{name}|]

    Meta.Struct id ver name fields -> [st|// #{name}|]

genInitializer :: T.Text
               -> Meta.Field
               -> T.Text
genInitializer prefix field = case field of
    Meta.Int8Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.UInt8Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.Int16Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.UInt16Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.Int32Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.UInt32Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.Int64Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.UInt64Field n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.Float32Field n v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.Float64Field n v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.ByteStringField n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.StringField n s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.NTStringField n s v c -> [st|// FIXME: null terminated string|]
    Meta.EnumerateField n t s v c -> [st|#{prefix}#{n}(#{n})|]
    Meta.EntityField n t v c -> [st|#{prefix}#{n}(#{n})|]

genToString :: T.Text
            -> Meta.Field
            -> T.Text
genToString prefix field = case field of
    Meta.Int8Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.UInt8Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.Int16Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.UInt16Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.Int32Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.UInt32Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.Int64Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.UInt64Field n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.Float32Field n v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.Float64Field n v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.ByteStringField n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.StringField n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.NTStringField n s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.EnumerateField n t s v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]
    Meta.EntityField n t v c -> [st|<< ", #{prefix}#{n}=" << v.#{prefix}#{n}|]

genCompare :: T.Text
           -> Meta.Field
           -> T.Text
genCompare prefix field = case field of
    Meta.Int8Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.UInt8Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.Int16Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.UInt16Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.Int32Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.UInt32Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.Int64Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.UInt64Field n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.Float32Field n v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.Float64Field n v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.ByteStringField n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.StringField n s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.NTStringField n s v c -> [st|strncmp(l.#{prefix}#{n}, r.#{prefix}#{n}, #{fromMaybe 0 s})|]
    Meta.EnumerateField n t s v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]
    Meta.EntityField n t v c -> [st|l.#{prefix}#{n} == r.#{prefix}#{n}|]


