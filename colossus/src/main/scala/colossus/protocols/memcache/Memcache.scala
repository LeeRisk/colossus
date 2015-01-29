package colossus
package protocols.memcache

import core._
import service._

import akka.util.{ByteString, ByteStringBuilder}
import java.util.zip.{Deflater, Inflater}

import parsing._
import DataSize._

/*
 * Memcache protocol for Colossus, implements a majority of the commands with the exception of some of the
 * administrative commands like stats. It could be easily added though.
 *
 * It's important to note that the memcache keys cannot be over 250 bytes and will be truncated
 *
 * We also make sure that the keys are "well formed", removing all control characters and spaces, which
 * are not allowed by Memcached
 *
 * I've grabbed a snippet from memcached docs about ttls found here: https://github.com/memcached/memcached/blob/master/doc/protocol.txt
 *
 * TTLs can be a unix timestamp for an integer that is no more than 30 days in seconds (2592000 seconds). If the TTL is greater
 * than 30 days in seconds, the server will consider it to be real Unix time value rather than an offset from current time. 
 * 
 */


object MemcacheCommand {
  val RN = ByteString("\r\n")
  val SP = ByteString(" ")

  private def FormatCommand(command: ByteString, key: ByteString, data: Option[ByteString] = None, ttl: Option[ByteString] = None, flags: Option[ByteString] = None): ByteString = {
    val b = new ByteStringBuilder
    b.append(command)
    b.append(SP)
    
    val k = key.filter(_ > ' ').slice(0, 250) // Strip all control characters and space from the key, limit to 250
    b.append(k)
    
    flags.foreach { f =>
      b.append(SP)
      b.append(f)
    }
    
    ttl.foreach { t =>
      b.append(SP)
      b.append(t)
    }

    data.foreach { d  =>
      b.append(SP)
      b.append(ByteString(s"${d.size}"))
      b.append(RN)
      b.append(d)
    }

    b.append(RN) 
    b.result
  }

  case class Get(key: ByteString) extends MemcacheCommand {
    def bytes (compressor: Compressor)= FormatCommand(ByteString("get"), key)
  }
  object Get {
    def apply(key: String): Get = Get(ByteString(key))
  }
  case class Set(key: ByteString,  value: ByteString, ttl: Integer) extends MemcacheCommand {
  
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("set"), key, Some(data), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }
  object Set {
    def apply(key: String, value: String, ttl: Integer = 0): Set = Set(ByteString(key), ByteString(value), ttl)
  }
  object Add {
    def apply(key: String, value: String, ttl: Integer = 0): Add = Add(ByteString(key), ByteString(value), ttl)
  }
  case class Add(key: ByteString,  value: ByteString, ttl: Integer) extends MemcacheCommand {
  
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("add"), key, Some(value), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }
  object Replace {
    def apply(key: String, value: String, ttl: Integer = 0): Replace = Replace(ByteString(key), ByteString(value), ttl)
  }
  case class Replace(key: ByteString, value: ByteString, ttl: Integer) extends MemcacheCommand {
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("replace"), key, Some(data), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }
  
  // Append does not take <flags> or <expiretime> but we have to provide them according to the protocol
  object Append {
    def apply(key: String, value: String, ttl: Integer = 0): Append = Append(ByteString(key), ByteString(value), ttl)
  }

  case class Append(key: ByteString, value: ByteString, ttl: Integer) extends MemcacheCommand {
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("append"), key, Some(data), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }

  // Prepend does not take <flags> or <expiretime> but we have to provide them according to the protocol
  object Prepend {
    def apply(key: String, value: String, ttl: Integer = 0): Prepend = Prepend(ByteString(key), ByteString(value), ttl)
  }
  case class Prepend(key: ByteString, value: ByteString, ttl: Integer) extends MemcacheCommand {
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("prepend"), key, Some(data), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }
  
  object Cas {
    def apply(key: String, value: String, ttl: Integer = 0): Cas = Cas(ByteString(key), ByteString(value), ttl)
  }
  case class Cas(key: ByteString, value: ByteString, ttl: Integer) extends MemcacheCommand {
    def bytes(compressor: Compressor) = {
      val data = compressor.compress(value)
      FormatCommand(ByteString("cas"), key, Some(data), Some(ByteString(s"${ttl}")), Some(ByteString(s"0")))
    }
  }
  case class Delete(key: ByteString) extends MemcacheCommand {
    def bytes(c: Compressor) = FormatCommand(ByteString("delete"), key)
  }
  object Delete {
    def apply(key: String): Delete = Delete(ByteString(key))
  }
  object Incr {
    def apply(key: String, value: Integer): Incr = Incr(ByteString(key), ByteString(s"${value}"))
  }
  case class Incr(key: ByteString, value: ByteString) extends MemcacheCommand {
    def bytes(c: Compressor) = ByteString("incr ") ++ key.filter(_ > ' ').slice(0,250) ++ ByteString(" ") ++ value ++ RN
  }
  object Decr {
    def apply(key: String, value: Integer): Decr = Decr(ByteString(key), ByteString(s"${value}")) 
  }
  case class Decr(key: ByteString, value: ByteString) extends MemcacheCommand {
    def bytes(c: Compressor) = ByteString("decr ") ++ key.filter(_ > ' ').slice(0,250) ++ ByteString(" ") ++ value ++ RN
  }
  object Touch {
    def apply(key: String, ttl: Integer): Touch = Touch(ByteString(key), ByteString(s"${ttl}"))
  }
  case class Touch(key: ByteString, ttl: ByteString) extends MemcacheCommand {
    def bytes(c: Compressor) = ByteString("touch ") ++ key.filter(_ > ' ').slice(0,250) ++ ByteString(" ") ++ ttl ++ RN
  }

}
import MemcacheCommand.RN
//todo, ttl's flags, etc
sealed trait MemcacheCommand {
  //compressor should only be used on DATA
  def bytes(compressor: Compressor): ByteString
  override def toString = bytes(NoCompressor).utf8String
}

sealed trait MemcacheReply
sealed trait MemcacheHeader
object MemcacheReply {
  sealed trait DataReply extends MemcacheReply 
    
  case class Value(key: String, data: ByteString) extends DataReply
  case class Values(values: Seq[Value]) extends DataReply
  case object NoData extends DataReply

  //these are all one-line responses
  sealed trait MemcacheError extends MemcacheReply with MemcacheHeader {
    def error: String
  }
  case class Error(error: String) extends MemcacheError
  case class ClientError(error: String) extends MemcacheError
  case class ServerError(error: String) extends MemcacheError

  case object Stored extends MemcacheReply with MemcacheHeader
  case object NotFound  extends MemcacheReply with MemcacheHeader
  case object Deleted extends MemcacheReply with MemcacheHeader
  case object NotStored extends MemcacheReply with MemcacheHeader
  case object Exists extends MemcacheReply with MemcacheHeader
  
}

class MemcacheReplyParser(maxSize: DataSize = MemcacheReplyParser.DefaultMaxSize) {

  private var parser = MemcacheReplyParser(maxSize)

  def parse(data: DataBuffer): Option[MemcacheReply] = parser.parse(data)

  def reset() {
    parser = MemcacheReplyParser.reply
  }
}

object MemcacheReplyParser {
  import parsing._
  import Combinators._
  import MemcacheReply._


  val DefaultMaxSize: DataSize = 1.MB

  def apply(size: DataSize = DefaultMaxSize) = maxSize(size, reply)

  def reply = delimitedString(' ', '\r') <~ byte |>{pieces => pieces.head match {
    case "VALUE"      => value(Nil, pieces(1), pieces(3).toInt)
    case "END"        => const(NoData)
    case "NOT_STORED" => const(NotStored)
    case "STORED"     => const(Stored)
    case "EXISTS"     => const(Exists)
    case "NOT_FOUND"  => const(NotFound)
    case "DELETED"    => const(Deleted)
    case "ERROR"      => const(Error("ERROR"))
    case other        => throw new ParseException(s"Unknown reply '$other'")
  }}
                                
  //returns either a Value or Values object depending if 1 or >1 values received
  def values(build: List[Value]): Parser[DataReply] = delimitedString(' ', '\r') <~ byte |>{pieces => pieces.head match {
    case "VALUE" => value(build, pieces(1), pieces(3).toInt)
    case "END" => const(if (build.size == 1) build.head else Values(build))
  }}

  //this parser starts immediately after the "VALUE" in the header, so it
  //parses the key and length in the header
  def value(build: List[Value], key: String, len: Int) = bytes(len) <~ bytes(2) |> {b => values(Value(key, b) :: build)}
}

case class MemcacheClientConfig(
  io: IOSystem,
  host: String,
  port: Int,
  compression: Boolean
)

trait Compressor {
  def compress(bytes: ByteString): ByteString
  def decompress(bytes: ByteString): ByteString
}

object NoCompressor extends Compressor{
  def compress(bytes: ByteString): ByteString = bytes
  def decompress(bytes: ByteString): ByteString = bytes
}

class ZCompressor(bufferKB: Int = 10) extends Compressor {
  val buffer = new Array[Byte](1024 * bufferKB)

  def compress(bytes: ByteString): ByteString = {
    val deflater = new Deflater
    deflater.setInput(bytes.toArray)
    deflater.finish
    val builder = new ByteStringBuilder
    var numread = 0
    do {
      numread = deflater.deflate(buffer)
      builder.putBytes(buffer, 0, numread)
    } while (numread > 0)
    deflater.end
    builder.result
  }

  def decompress(bytes: ByteString): ByteString = {
    val inflater = new Inflater
    inflater.setInput(bytes.toArray)
    val builder = new ByteStringBuilder
    var numread = 0
    do {
      numread = inflater.inflate(buffer)
      builder.putBytes(buffer, 0, numread)
    } while (numread > 0)
    inflater.end
    builder.result
  }

}



class MemcacheClientCodec(maxSize: DataSize = MemcacheReplyParser.DefaultMaxSize) extends Codec.ClientCodec[MemcacheCommand, MemcacheReply] {
  private var parser = new MemcacheReplyParser(maxSize)//(NoCompressor) //config

  def encode(cmd: MemcacheCommand): DataBuffer = DataBuffer(cmd.bytes(NoCompressor))
  def decode(data: DataBuffer): Option[DecodedResult[MemcacheReply]] = DecodedResult.static(parser.parse(data))
  def reset(){
    parser = new MemcacheReplyParser(maxSize)//(NoCompressor)
  }
}

class MemcacheClient(config: ClientConfig, maxSize : DataSize = MemcacheReplyParser.DefaultMaxSize)
  extends ServiceClient[MemcacheCommand, MemcacheReply](
    codec   = new MemcacheClientCodec(maxSize),
    config  = config
  )

