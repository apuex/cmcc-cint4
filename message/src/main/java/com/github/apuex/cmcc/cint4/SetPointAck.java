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
 * 写数据响应
 *
 * @author Wangxy
 */
public class SetPointAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public SetPointAck() {
        super(EnumPKType.SET_POINT_ACK);
    }

    public SetPointAck
    ( TID Id
    , EnumResult Result
    ) {
        super(EnumPKType.SET_POINT_ACK);
        this.Id = Id;
        this.Result = Result;
    }

    public SetPointAck
    ( int SerialNo
    , TID Id
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SET_POINT_ACK);
        this.Id = Id;
        this.Result = Result;
    }

    @Override
    public boolean equals(Object o) {
        SetPointAck r = null;
        if(o instanceof SetPointAck) {
            r = (SetPointAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Id.equals(r.Id)
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SetPointAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Id=").append(this.Id)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public TID Id; // 相应的值，数据的值的类型由相应的数据结构决定，数据结构中已经包含了监控点ID，因此上面的ID是冗余的
    public EnumResult Result; // 报文返回结果
}

