package com.larroy.utils

import org.specs2.mutable._
import org.specs2.matcher
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder._


class VarintSpec extends Specification with matcher.DataTables {
  "Varint" should {
    "encode and decode SignedInt" in {
      val xs = List(0,1,4,-1,0x7fffffff, -2, -5)
      val buff = new ArrayBuffer[Byte]()
      for (x <- xs) {
        Varint.writeSignedInt(x, buff)
      }
      val bbuff = ByteBuffer.wrap(buff.toArray).order(LITTLE_ENDIAN)
      for (x <- xs) {
        val r = Varint.readSignedInt(bbuff)
        r shouldEqual x
      }
      true
    }

    "encode and decode SignedLong" in {
      val xs = List[Long](0,1,4,-1,0x7fffffff, -2, -5, 0x7fffffffffffffffL)
      val buff = new ArrayBuffer[Byte]()
      for (x <- xs) {
        Varint.writeSignedLong(x, buff)
      }
      val bbuff = ByteBuffer.wrap(buff.toArray).order(LITTLE_ENDIAN)
      for (x <- xs) {
        val r = Varint.readSignedLong(bbuff)
        r shouldEqual x
      }
      true
    }

    "encode and decode UnsignedInt" in {
      val xs = List(0,1,4,-1,0x7fffffff, -2, -5)
      val buff = new ArrayBuffer[Byte]()
      for (x <- xs) {
        Varint.writeUnsignedInt(x, buff)
      }
      val bbuff = ByteBuffer.wrap(buff.toArray).order(LITTLE_ENDIAN)
      for (x <- xs) {
        val r = Varint.readUnsignedInt(bbuff)
        r shouldEqual x
      }
      true
    }

    "encode and decode UnsignedLong" in {
      val xs = List[Long](0,1,4,-1,0x7fffffff, -2, -5)
      val buff = new ArrayBuffer[Byte]()
      for (x <- xs) {
        Varint.writeUnsignedLong(x, buff)
      }
      val bbuff = ByteBuffer.wrap(buff.toArray).order(LITTLE_ENDIAN)
      for (x <- xs) {
        val r = Varint.readUnsignedLong(bbuff)
        r shouldEqual x
      }
      true
    }

  }
}


