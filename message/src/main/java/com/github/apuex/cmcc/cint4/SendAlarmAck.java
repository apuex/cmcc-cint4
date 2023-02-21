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

/**
 * 实时告警发送确认
 *
 * @author Wangxy
 */
public class SendAlarmAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public SendAlarmAck() {
        super(EnumPKType.SEND_ALARM_ACK);
    }

    public SendAlarmAck
    ( TAlarmArray Values
    ) {
        super(EnumPKType.SEND_ALARM_ACK);
        this.Values = Values;
    }

    public SendAlarmAck
    ( int SerialNo
    , TAlarmArray Values
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM_ACK);
        this.Values = Values;
    }

    @Override
    public boolean equals(Object o) {
        SendAlarmAck r = null;
        if(o instanceof SendAlarmAck) {
            r = (SendAlarmAck) o;
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
            .append("SendAlarmAck { ")
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

