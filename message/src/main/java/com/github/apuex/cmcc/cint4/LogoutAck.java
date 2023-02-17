package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登出响应
 */
public class LogoutAck extends Message { 
    public LogoutAck
    ( 
    ) {
        super(EnumPKType.LOGOUT_ACK);
        
    }

    public LogoutAck
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.LOGOUT_ACK);
        
    }

    public static void encode(ByteBuffer buf, LogoutAck v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        
        buf.putShort(v.CRC16);
    }

    public static LogoutAck decode(ByteBuffer buf) {
        LogoutAck v = new LogoutAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

