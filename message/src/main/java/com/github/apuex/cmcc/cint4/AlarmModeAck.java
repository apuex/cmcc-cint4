package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警方式设置响应
 */
public class AlarmModeAck extends Message {
    public AlarmModeAck
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public AlarmModeAck
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

