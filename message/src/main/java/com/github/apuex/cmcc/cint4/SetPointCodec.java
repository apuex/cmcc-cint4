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
 * 写数据请求
 *
 * @author Wangxy
 */
public class SetPointCodec {

	public SetPointCodec(TATDCodec ValueCodec) {
		this.ValueCodec = ValueCodec;
	}
	
    public void encode(ByteBuffer buf, SetPoint v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        this.ValueCodec.encode(buf, v.Value);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        // Message LENGTH - envelope fields
        final int pos = buf.position();
        v.Length = pos - initialPos;
        buf.position(initialPos + 4); 
        buf.putInt(v.Length);
        buf.position(pos - 2); 
        v.CRC16 = Util.CRC16(buf.array(), initialPos + 4, pos - 2); 
        buf.putShort(v.CRC16);
    }

    public SetPoint decode(ByteBuffer buf) {
        final int initialPos = buf.position();
        SetPoint v = new SetPoint();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Value = this.ValueCodec.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.position(initialPos + v.Length - 2);
        v.CRC16 = buf.getShort();
        return v;
    }

    public TATDCodec ValueCodec; // 5.1.8中的TA/TD的数据结构定义
}

