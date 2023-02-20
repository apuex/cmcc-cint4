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
 * 实时告警发送
 *
 * @author Wangxy
 */
public class SendAlarm extends Message {
    private static final long serialVersionUID = 1L;
    
    public SendAlarm() {
        super(EnumPKType.SEND_ALARM);
    }

    public SendAlarm
    ( TAlarmArray Values
    ) {
        super(EnumPKType.SEND_ALARM);
        this.Values = Values;
    }

    public SendAlarm
    ( int SerialNo
    , TAlarmArray Values
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM);
        this.Values = Values;
    }

    public static void encode(ByteBuffer buf, SendAlarm v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        TAlarmArray.encode(buf, v.Values);
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

    public static SendAlarm decode(ByteBuffer buf) {
        SendAlarm v = new SendAlarm();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Values = TAlarmArray.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SendAlarm r = null;
        if(o instanceof SendAlarm) {
            r = (SendAlarm) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Values.equals(r.Values)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SendAlarm { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Values=").append(this.Values)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public TAlarmArray Values; // 告警信息数量及清单。见TAlarm的数据结构定义
}

