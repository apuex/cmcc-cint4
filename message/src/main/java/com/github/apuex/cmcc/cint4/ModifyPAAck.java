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
 * 改口令响应
 *
 * @author Wangxy
 */
public class ModifyPAAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public ModifyPAAck() {
        super(EnumPKType.MODIFY_PA_ACK);
    }

    public ModifyPAAck
    ( EnumResult Result
    ) {
        super(EnumPKType.MODIFY_PA_ACK);
        this.Result = Result;
    }

    public ModifyPAAck
    ( int SerialNo
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.MODIFY_PA_ACK);
        this.Result = Result;
    }

    @Override
    public boolean equals(Object o) {
        ModifyPAAck r = null;
        if(o instanceof ModifyPAAck) {
            r = (ModifyPAAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("ModifyPAAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public EnumResult Result; // 报文返回结果
}

