package colossus

import org.scalatest._

import akka.util.ByteString

import protocols.memcache.MemcacheCommand._ 
import protocols.memcache.NoCompressor


class MemcacheCommandSuite extends FlatSpec with ShouldMatchers{
  "MemcacheCommand" should "format a GET correctly" in {
    val experimental = Get("test")
    //experimental.bytes(NoCompressor) should equal(ByteString("get test\r\n")) 
    experimental.toString() should equal("get test\r\n")
  }

  it should "format a SET correctly" in {
    val experimental = Set("key", "value", 30) // key, value, ttl
    //experimental.bytes(NoCompressor) should equal(ByteString("set key 0 30 5\r\nvalue\r\n"))
    experimental.toString() should equal("set key 0 30 5\r\nvalue\r\n")
  }

  it should "format an ADD correctly" in {
    val experimental = Add("key", "magic", 30)
    //experimental.bytes(NoCompressor) should equal(ByteString("add key 0 30 5\r\nmagic\r\n"))
    experimental.toString() should equal("add key 0 30 5\r\nmagic\r\n")
  }

  it should "format a REPLACE correctly" in {
    val experimental = Replace("key", "magic", 30)
    //experimental.bytes(NoCompressor) should equal(ByteString("replace key 0 30 5\r\nmagic\r\n"))
    experimental.toString() should equal("replace key 0 30 5\r\nmagic\r\n")
  }
  
  it should "format an APPEND correctly" in {
    val experimental = Append("key", "magic", 30)
    //experimental.bytes(NoCompressor) should equal(ByteString("append key 0 30 5\r\nmagic\r\n"))
    experimental.toString() should equal("append key 0 30 5\r\nmagic\r\n")
  }

  it should "format a PREPEND correctly" in {
    val experimental = Prepend("key", "magic", 30)
    //experimental.bytes(NoCompressor) should equal(ByteString("prepend key 0 30 5\r\nmagic\r\n"))
    experimental.toString() should equal("prepend key 0 30 5\r\nmagic\r\n")
  }

  it should "format a CAS correctly" in {
    val experimental = Cas("key", "magic", 30)
    //experimental.bytes(NoCompressor) should equal(ByteString("cas key 0 30 5\r\nmagic\r\n"))
    experimental.toString() should equal("cas key 0 30 5\r\nmagic\r\n")
  }

  it should "format DELETE correctly" in {
    val experimental = Delete("key")
    experimental.bytes(NoCompressor) should equal(ByteString("delete key\r\n"))
    experimental.toString() should equal("delete key\r\n")
  }

  it should "format INCR correctly" in {
    val experimental = Incr("key", 1)
    //experimental.bytes(NoCompressor) should equal(ByteString("incr key 1\r\n"))
    experimental.toString() should equal("incr key 1\r\n")
  }

  it should "format DECR correctly" in {
    val experimental = Decr("key", 1)
    experimental.bytes(NoCompressor) should equal(ByteString("decr key 1\r\n"))
    experimental.toString() should equal("decr key 1\r\n")
  }

  it should "format TOUCH correctly" in {
    val experimental = Touch("key", 30)
    experimental.bytes(NoCompressor) should equal(ByteString("touch key 30\r\n"))
    experimental.toString() should equal("touch key 30\r\n")
  }

}
