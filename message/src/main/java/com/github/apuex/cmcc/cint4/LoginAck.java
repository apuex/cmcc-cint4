package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登录响应
 */
public class LoginAck extends Message { 
    public LoginAck() {
        super(EnumPKType.LOGIN_ACK);
    }

    public LoginAck
    ( EnumRightLevel RightLevel
    ) {
        super(EnumPKType.LOGIN_ACK);
        this.RightLevel = RightLevel;
    }

    public LoginAck
    ( int SerialNo
    , EnumRightLevel RightLevel
    ) {
        super(SerialNo, EnumPKType.LOGIN_ACK);
        this.RightLevel = RightLevel;
    }

    public static void encode(ByteBuffer buf, LoginAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.RightLevel.getValue());
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

    public static LoginAck decode(ByteBuffer buf) {
        LoginAck v = new LoginAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.RightLevel = EnumRightLevel.fromValue(buf.getInt());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public EnumRightLevel RightLevel; // 监控系统下级SC向上级SC提供的权限定义
}

