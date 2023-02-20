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
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos - 2));
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

    @Override
    public boolean equals(Object o) {
        LoginAck r = null;
        if(o instanceof LoginAck) {
            r = (LoginAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.RightLevel == r.RightLevel
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("LoginAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("RightLevel=").append(this.RightLevel)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public EnumRightLevel RightLevel; // 监控系统下级SC向上级SC提供的权限定义
}

