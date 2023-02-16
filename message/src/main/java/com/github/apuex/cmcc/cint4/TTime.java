package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 时间的结构
 */
public class TTime implements Serializable {
    public short Years; // 年
    public byte Month; // 月
    public byte Day; // 日
    public byte Hour; // 时
    public byte Minute; // 分
    public byte Second; // 秒
}

