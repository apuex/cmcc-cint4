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

/**
 * 实时数据响应
 *
 * @author Wangxy
 */
public class DynAccessModeAckCodec {

	public DynAccessModeAckCodec(TATDArrayCodec Values1Codec) {
		this.Values1Codec = Values1Codec;
	}
	
    public void encode(ByteBuffer buf, DynAccessModeAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.TerminalID);
        buf.putInt(v.GroupID);
        buf.putInt(v.Result.getValue());
        this.Values1Codec.encode(buf, v.Values1);
        this.Values2Codec.encode(buf, v.Values2);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
        v.Length = pos - initialPos;
        buf.position(initialPos + 4);
        buf.putInt(v.Length);
        buf.position(pos - 2);
        v.CRC16 = Util.CRC16(buf.array(), initialPos, pos - 2);
        buf.putShort(v.CRC16);
    }

    public DynAccessModeAck decode(ByteBuffer buf) {
        DynAccessModeAck v = new DynAccessModeAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.TerminalID = buf.getInt();
        v.GroupID = buf.getInt();
        v.Result = EnumResult.fromValue(buf.getInt());
        v.Values1 = this.Values1Codec.decode(buf);
        v.Values2 = this.Values2Codec.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public final TATDArrayCodec Values1Codec; // 返回正确数据值得数量及值对应对应的值5.1.8中的TA/TD的数据结构定义
    public final TIDArrayCodec Values2Codec = new TIDArrayCodec(); // 返回无效监控点ID的数量，如果返回0则所有数据有效，Values2为空及对应的无效的监控点ID
}

