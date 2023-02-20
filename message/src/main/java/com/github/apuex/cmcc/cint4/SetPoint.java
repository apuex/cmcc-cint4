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
public class SetPoint extends Message {
    private static final long serialVersionUID = 1L;
    
    public SetPoint() {
        super(EnumPKType.SET_POINT);
    }

    public SetPoint
    ( TATD Value
    ) {
        super(EnumPKType.SET_POINT);
        this.Value = Value;
    }

    public SetPoint
    ( int SerialNo
    , TATD Value
    ) {
        super(SerialNo, EnumPKType.SET_POINT);
        this.Value = Value;
    }

    public static void encode(ByteBuffer buf, SetPoint v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        TATD.encode(buf, v.Value);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos - 2));
    }

    public static SetPoint decode(ByteBuffer buf) {
        SetPoint v = new SetPoint();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Value = TATD.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SetPoint r = null;
        if(o instanceof SetPoint) {
            r = (SetPoint) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Value.equals(r.Value)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SetPoint { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Value=").append(this.Value)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public TATD Value; // 5.1.8中的TA/TD的数据结构定义
}

