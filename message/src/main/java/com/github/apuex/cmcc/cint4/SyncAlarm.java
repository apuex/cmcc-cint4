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
 * 告警同步
 *
 * @author Wangxy
 */
public class SyncAlarm extends Message {
    private static final long serialVersionUID = 1L;
    
    public SyncAlarm
    ( 
    ) {
        super(EnumPKType.SYNC_ALARM);
        
    }

    public SyncAlarm
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.SYNC_ALARM);
        
    }

    @Override
    public boolean equals(Object o) {
        SyncAlarm r = null;
        if(o instanceof SyncAlarm) {
            r = (SyncAlarm) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SyncAlarm { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    
}

