/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;

public class DynAccessModeAckCodecTest {
    @Test
    public void testEncode() {
        byte[] expected = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x52, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x92, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x03, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38
            , (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x40, (byte)0xE2, (byte)0x01
            , (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x39, (byte)0x30, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31
            , (byte)0x20, (byte)0x20, (byte)0x79, (byte)0xE9, (byte)0xF6, (byte)0x42, (byte)0x01, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x04, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35
            , (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x37, (byte)0x38, (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x03
            , (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34
            , (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x37, (byte)0x39, (byte)0x30, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20
            , (byte)0xEA, (byte)0xDF
            };
        List<TATD> vl = new LinkedList<TATD>();
        TD td = new TD();
        td.SiteID = "123";
        td.DeviceID = "456";
        td.SignalID = "789";
        td.SignalNumber = "1";
        td.Value = 123456;
        td.Status = EnumState.CRITICAL;
        vl.add(td);
        TA ta = new TA();
        ta.SiteID = "123";
        ta.DeviceID = "456";
        ta.SignalID = "790";
        ta.SignalNumber = "1";
        ta.Value = 123.456f;
        ta.Status = EnumState.CRITICAL;
        vl.add(ta);
        TATDArray Values1 = new TATDArray(vl);

        List<TID> idl = new LinkedList<TID>();
        TID tid1 = new TID();
        tid1.Type = EnumType.DI;
        tid1.SiteID = "123";
        tid1.DeviceID = "456";
        tid1.SignalID = "789";
        tid1.SignalNumber = "1";
        idl.add(tid1);
        TID tid2 = new TID();
        tid2.Type = EnumType.AI;
        tid2.SiteID = "123";
        tid2.DeviceID = "456";
        tid2.SignalID = "790";
        tid2.SignalNumber = "1";
        idl.add(tid2);
        
        TIDArray Values2 = new TIDArray(idl);
        DynAccessModeAck v = new DynAccessModeAck(1, 2, 3, EnumResult.SUCCESS, Values1, Values2);
      	byte[] actual = new byte[338];
      	ByteBuffer buf = ByteBuffer.wrap(actual);
      	buf.order(ByteOrder.LITTLE_ENDIAN);

      	Map<TID, EnumType> typeMap = new TreeMap<TID, EnumType>(new TIDComparator());
      	TATDCodec tatdCodec = new TATDCodec(tid -> typeMap.get(tid));
      	TATDArrayCodec Values1Codec = new TATDArrayCodec(tatdCodec);
      	DynAccessModeAckCodec codec = new DynAccessModeAckCodec(Values1Codec);
      	codec.encode(buf, v);

        Util.printBytes("actual", actual);

      	Assert.assertArrayEquals(expected, actual);
    }

    @Test
    public void testDecode() {
        byte[] input = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x52, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x92, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x03, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38
            , (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x40, (byte)0xE2, (byte)0x01
            , (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x39, (byte)0x30, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31
            , (byte)0x20, (byte)0x20, (byte)0x79, (byte)0xE9, (byte)0xF6, (byte)0x42, (byte)0x01, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x04, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35
            , (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x37, (byte)0x38, (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x03
            , (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34
            , (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x37, (byte)0x39, (byte)0x30, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20
            , (byte)0xEA, (byte)0xDF
            };
        List<TATD> vl = new LinkedList<TATD>();
        TD td = new TD();
        td.SiteID = "123";
        td.DeviceID = "456";
        td.SignalID = "789";
        td.SignalNumber = "1";
        td.Value = 123456;
        td.Status = EnumState.CRITICAL;
        vl.add(td);
        TA ta = new TA();
        ta.SiteID = "123";
        ta.DeviceID = "456";
        ta.SignalID = "790";
        ta.SignalNumber = "1";
        ta.Value = 123.456f;
        ta.Status = EnumState.CRITICAL;
        vl.add(ta);
        TATDArray Values1 = new TATDArray(vl);

        List<TID> idl = new LinkedList<TID>();
        TID tid1 = new TID();
        tid1.Type = EnumType.DI;
        tid1.SiteID = "123";
        tid1.DeviceID = "456";
        tid1.SignalID = "789";
        tid1.SignalNumber = "1";
        idl.add(tid1);
        TID tid2 = new TID();
        tid2.Type = EnumType.AI;
        tid2.SiteID = "123";
        tid2.DeviceID = "456";
        tid2.SignalID = "790";
        tid2.SignalNumber = "1";
        idl.add(tid2);
        
        TIDArray Values2 = new TIDArray(idl);
        DynAccessModeAck expected = new DynAccessModeAck(1, 2, 3, EnumResult.SUCCESS, Values1, Values2);
      	expected.Length = input.length;
      	expected.CRC16 = (short)0xDFEA;

      	Map<TID, EnumType> typeMap = new TreeMap<TID, EnumType>(new TIDComparator());
      	for(TATD v : vl) {
            TID tid = v.toTID();
      		typeMap.put(tid, tid.Type);
      	}
      	TATDCodec tatdCodec = new TATDCodec(tid -> typeMap.get(tid));
      	TATDArrayCodec Values1Codec = new TATDArrayCodec(tatdCodec);
      	DynAccessModeAckCodec codec = new DynAccessModeAckCodec(Values1Codec);

        
      	ByteBuffer buf = ByteBuffer.wrap(input);
      	buf.order(ByteOrder.LITTLE_ENDIAN);
        DynAccessModeAck actual = codec.decode(buf);

      	Assert.assertEquals(expected, actual);
    }
}

