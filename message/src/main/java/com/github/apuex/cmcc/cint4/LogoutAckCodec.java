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
 * 登出响应
 *
 * @author Wangxy
 */
public class LogoutAckCodec {

    public void encode(ByteBuffer buf, LogoutAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        
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

    public LogoutAck decode(ByteBuffer buf) {
        final int initialPos = buf.position();
        LogoutAck v = new LogoutAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.position(initialPos + v.Length - 2);
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

