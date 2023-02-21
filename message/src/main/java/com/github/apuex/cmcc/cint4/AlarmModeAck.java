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
 * 告警方式设置响应
 *
 * @author Wangxy
 */
public class AlarmModeAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public AlarmModeAck() {
        super(EnumPKType.ALARM_MODE_ACK);
    }

    public AlarmModeAck
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public AlarmModeAck
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    @Override
    public boolean equals(Object o) {
        AlarmModeAck r = null;
        if(o instanceof AlarmModeAck) {
            r = (AlarmModeAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.GroupID == r.GroupID
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("AlarmModeAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("GroupID=").append(this.GroupID)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

