package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时告警发送
 */
public class SendAlarm extends Message {
    public SendAlarm
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.SEND_ALARM);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public SendAlarm
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

