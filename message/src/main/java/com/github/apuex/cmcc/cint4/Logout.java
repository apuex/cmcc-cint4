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
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos));
    }

    public static Logout decode(ByteBuffer buf) {
        Logout v = new Logout();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

