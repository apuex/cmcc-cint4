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
 * 写数据响应
 *
 * @author Wangxy
 */
public class SetPointAckCodec {

    public void encode(ByteBuffer buf, SetPointAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        this.IdCodec.encode(buf, v.Id);
        buf.putInt(v.Result.getValue());
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

    public SetPointAck decode(ByteBuffer buf) {
        final int initialPos = buf.position();
        SetPointAck v = new SetPointAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Id = this.IdCodec.decode(buf);
        v.Result = EnumResult.fromValue(buf.getInt());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.position(initialPos + v.Length - 2);
        v.CRC16 = buf.getShort();
        return v;
    }

    public TIDCodec IdCodec = new TIDCodec(); // 相应的值，数据的值的类型由相应的数据结构决定，数据结构中已经包含了监控点ID，因此上面的ID是冗余的
}

