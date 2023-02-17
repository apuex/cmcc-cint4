package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

public class Message implements Serializable {
    public Message
    ( EnumPKType PKType
    ) {
        this.Header = 0x7E7C6B5A;
        this.Length = 0;
        this.SerialNo = 0;
        this.PKType = PKType;
        this.CRC16 = 0;
    }

    public Message
    ( int SerialNo
    , EnumPKType PKType
    ) {
        this.Header = 0x7E7C6B5A;
        this.Length = 0;
        this.SerialNo = SerialNo;
        this.PKType = PKType;
        this.CRC16 = 0;
    }

    public int Header; // Message header, 0x7E7C6B5A.
    public int Length; // Message length.
    public int SerialNo; // Message serial number.
    public EnumPKType PKType; // 报文定义
    public short CRC16; // Message CRC16.
}

