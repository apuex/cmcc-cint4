package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时数据响应
 */
public class DynAccessModeAck extends Message { 
    public DynAccessModeAck() {
        super(EnumPKType.DYN_ACCESS_MODE_ACK);
    }

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

    public static void encode(ByteBuffer buf, DynAccessModeAck v) {
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.TerminalID);
        buf.putInt(v.GroupID);
        buf.putInt(v.Result.getValue());
        buf.putInt(v.PollingTime);
        TATDArray.encode(buf, v.Values1);
        TIDArray.encode(buf, v.Values2);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
    }

    public static DynAccessModeAck decode(ByteBuffer buf) {
        DynAccessModeAck v = new DynAccessModeAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.TerminalID = buf.getInt();
        v.GroupID = buf.getInt();
        v.Result = EnumResult.fromValue(buf.getInt());
        v.PollingTime = buf.getInt();
        v.Values1 = TATDArray.decode(buf);
        v.Values2 = TIDArray.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public int TerminalID; // 上级SCID
    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
    public int PollingTime; // 定时方式时的发送间隔秒数，小于1无效，若出现小于1的值，则按等于1处理。
    public TATDArray Values1; // 返回正确数据值得数量及值对应对应的值5.1.8中的TA/TD的数据结构定义
    public TIDArray Values2; // 返回无效监控点ID的数量，如果返回0则所有数据有效，Values2为空及对应的无效的监控点ID
}

