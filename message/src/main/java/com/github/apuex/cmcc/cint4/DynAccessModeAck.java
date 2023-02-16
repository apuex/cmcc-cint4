package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时数据响应
 */
public class DynAccessModeAck extends Message {
    public DynAccessModeAck
    ( int TerminalID
    , int GroupID
    , EnumResult Result
    , int PollingTime
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.PollingTime = PollingTime;
        this.Values1 = Values1;
        this.Values2 = Values2;
    }

    public DynAccessModeAck
    ( int SerialNo
    , int TerminalID
    , int GroupID
    , EnumResult Result
    , int PollingTime
    , TATDArray Values1
    , TIDArray Values2
    ) {
        super(SerialNo, EnumPKType.DYN_ACCESS_MODE_ACK);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Result = Result;
        this.PollingTime = PollingTime;
        this.Values1 = Values1;
        this.Values2 = Values2;
    }

    public int TerminalID; // 上级SCID
    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
    public int PollingTime; // 定时方式时的发送间隔秒数，小于1无效，若出现小于1的值，则按等于1处理。
    public TATDArray Values1; // 返回正确数据值得数量及值对应对应的值5.1.8中的TA/TD的数据结构定义
    public TIDArray Values2; // 返回无效监控点ID的数量，如果返回0则所有数据有效，Values2为空及对应的无效的监控点ID
}

