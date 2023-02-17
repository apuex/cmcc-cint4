package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登出
 */
public class Logout extends Message { 
    public Logout
    ( 
    ) {
        super(EnumPKType.LOGOUT);
        
    }

    public Logout
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.LOGOUT);
        
    }

    public static void encode(ByteBuffer buf, Logout v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        
        buf.putShort(v.CRC16);
    }

    public static Logout decode(ByteBuffer buf) {
        Logout v = new Logout();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

