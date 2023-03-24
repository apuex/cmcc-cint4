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
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.Values1 = Values1;
        this.Values2 = Values2;
    }

    public DynAccessModeAck
    ( int SerialNo
    , int TerminalID
    , int GroupID
    , EnumResult Result
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(SerialNo, EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.Values1 = Values1;
        this.Values2 = Values2;
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
            .append(", ").append("Values1=").append(this.Values1)
            .append(", ").append("Values2=").append(this.Values2)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public int TerminalID; // 上级SCID
    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
    public TATDArray Values1; // 返回正确数据值得数量及值对应对应的值5.1.8中的TA/TD的数据结构定义
    public TIDArray Values2; // 返回无效监控点ID的数量，如果返回0则所有数据有效，Values2为空及对应的无效的监控点ID
}

