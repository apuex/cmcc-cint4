package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登录响应
 */
public class LoginAck extends Message {
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

    public EnumRightLevel RightLevel; // 监控系统下级SC向上级SC提供的权限定义
}

