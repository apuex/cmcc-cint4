package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时告警发送确认
 */
public class SendAlarmAck extends Message {
    public SendAlarmAck
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.SEND_ALARM_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public SendAlarmAck
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

