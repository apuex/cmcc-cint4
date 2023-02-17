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
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.RightLevel.getValue());
        buf.putShort(v.CRC16);
    }

    public static LoginAck decode(ByteBuffer buf) {
        LoginAck v = new LoginAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.RightLevel = EnumRightLevel.fromValue(buf.getInt());
        v.CRC16 = buf.getShort();
        return v;
    }

    public EnumRightLevel RightLevel; // 监控系统下级SC向上级SC提供的权限定义
}

