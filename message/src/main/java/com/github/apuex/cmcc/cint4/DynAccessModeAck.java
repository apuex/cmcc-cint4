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
public class DynAccessModeAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public DynAccessModeAck() {
        super(EnumPKType.DYN_ACCESS_MODE_ACK);
    }

    public DynAccessModeAck
    ( int TerminalID
    , int GroupID
    , EnumResult Result
    , int PollingTime
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.PollingTime = PollingTime;
        this.Values1 = Values1;
        this.Values2 = Values2;
    }

    public DynAccessModeAck
    ( int SerialNo
    , int TerminalID
    , int GroupID
    , EnumResult Result
    , int PollingTime
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(SerialNo, EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.PollingTime = PollingTime;
        this.Values1 = Values1;
        this.Values2 = Values2;
    }

    public static void encode(ByteBuffer buf, DynAccessModeAck v) {
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
        buf.putInt(v.PollingTime);
        TATDArray.encode(buf, v.Values1);
        TIDArray.encode(buf, v.Values2);
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

    public static DynAccessModeAck decode(ByteBuffer buf) {
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
        v.PollingTime = buf.getInt();
        v.Values1 = TATDArray.decode(buf);
        v.Values2 = TIDArray.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        DynAccessModeAck r = null;
        if(o instanceof DynAccessModeAck) {
            r = (DynAccessModeAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.TerminalID == r.TerminalID
            && this.GroupID == r.GroupID
            && this.Result == r.Result
            && this.PollingTime == r.PollingTime
            && this.Values1.equals(r.Values1)
            && this.Values2.equals(r.Values2)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("DynAccessModeAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("TerminalID=").append(this.TerminalID)
            .append(", ").append("GroupID=").append(this.GroupID)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("PollingTime=").append(this.PollingTime)
            .append(", ").append("Values1=").append(this.Values1)
            .append(", ").append("Values2=").append(this.Values2)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public int TerminalID; // 上级SCID
    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
    public int PollingTime; // 定时方式时的发送间隔秒数，小于1无效，若出现小于1的值，则按等于1处理。
    public TATDArray Values1; // 返回正确数据值得数量及值对应对应的值5.1.8中的TA/TD的数据结构定义
    public TIDArray Values2; // 返回无效监控点ID的数量，如果返回0则所有数据有效，Values2为空及对应的无效的监控点ID
}

